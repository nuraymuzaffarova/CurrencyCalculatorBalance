import scala.collection.immutable.{List, Map}

case class Transaction(ccy: Currency, var amount: Int)
case class Currency(key: String)

object CurrencyBalanceCalculator {

  var transactionsMap = Map[Currency, Int]()
  var ccyBalances = List[Transaction]()
  final val acceptedCurrencies = Seq(Currency("USD"), Currency("HKD"), Currency("CNY"), Currency("NZD"), Currency("GBP"), Currency("EUR"))
  val exchangeRates: Map[Currency, Double] = Map(Currency("HKD") -> 0.13, Currency("CNY") -> 0.15, Currency("NZD") -> 0.62, Currency("GBP") -> 1.19, Currency("EUR") -> 1.02)

  def executeTransactions(ccy: Currency, amount: Int) = {
    if (transactionsMap.contains(ccy)) {
      ccyBalances = ccyBalances.filter(_.ccy != ccy)
      val curBalance = transactionsMap(ccy)
      val curTransaction = Transaction(ccy, curBalance + amount)
      transactionsMap = transactionsMap + (ccy -> (curBalance + amount))
      ccyBalances = ccyBalances :+ curTransaction
    } else {
      val t = Transaction(ccy, amount)
      ccyBalances = ccyBalances :+ t
      transactionsMap = transactionsMap + (ccy -> amount)
    }
  }

  def processInput(input: String): Option[(String, Int)] = {
    val splitted = input.split(" ")
    if(splitted.length == 2) {
      if(splitted(0).toIntOption.isDefined && acceptedCurrencies.contains(Currency(splitted(1)))) {
        Some(splitted(1), splitted(0).toInt)
      }
      else if(splitted(1).toIntOption.isDefined && acceptedCurrencies.contains(Currency(splitted(0)))) {
        Some(splitted(0), splitted(1).toInt)
      }
      else None
    }
    else None
  }

}




