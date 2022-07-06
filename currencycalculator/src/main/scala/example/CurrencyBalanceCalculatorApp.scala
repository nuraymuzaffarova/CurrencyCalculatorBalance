import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

object CurrencyBalanceCalculatorApp extends App {

    val ccyBalanceCalc = CurrencyBalanceCalculator

    class PublishTask extends TimerTask {
      @Override def run() = {
        ccyBalanceCalc.ccyBalances.filter(_.amount != 0)
          .map(b => {
            val defaultOutput = b.ccy.key.toUpperCase() ++ " " ++ b.amount.toString
            b.ccy match {
            case Currency("USD") => println(defaultOutput)
            case _               => println(defaultOutput ++ " (USD " ++ s"${ccyBalanceCalc.exchangeRates(b.ccy) * b.amount})")
          }}
          )
      }
    }

    val executor = Executors.newScheduledThreadPool(1)
    executor.scheduleAtFixedRate(new PublishTask, 0, 60, TimeUnit.SECONDS)
    if (args.length == 0) {
      while (true) {
        val userInput = scala.io.StdIn.readLine()
        if (userInput.equals("quit")) {
          System.exit(0)
          executor.shutdown()
        }
        else if (ccyBalanceCalc.processInput(userInput.toUpperCase()).isDefined) {
            ccyBalanceCalc.processInput(userInput.toUpperCase()) match {
            case Some((ccy, amount)) => ccyBalanceCalc.executeTransactions(Currency(ccy), amount)
            case None => println("Invalid Input")
          }
        }
        else println("Invalid Input")
      }
    }
    else {
      val fileReader = scala.io.Source.fromFile(args(0))
      try {
        for (line <- fileReader.getLines) {
            ccyBalanceCalc.processInput(line) match {
              case Some((ccy, amount)) => ccyBalanceCalc.executeTransactions(Currency(ccy), amount)
              case None => println("Invalid Input")
            }
          }
      }
      finally fileReader.close()
    }
}

