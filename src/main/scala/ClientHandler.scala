package ClientHandler
import collection.mutable


class ClientHandler(val name: String,
                    var balance: Double = 0,
                    val currencies: mutable.Map[String, Int] = mutable.Map.empty) {
  
  /**
    * Operation, that allows client to sell currencies.
    */
  def sell(currency: String, count: Int, price: Double)= {
    gainBalance(count * price)
    wasteCurrency(currency, count)
    this
  }
  
  /**
    * Operation, that allows client to buy currencies.
    */
  def buy(currency : String, count : Int, price : Double) = {
    wasteBalance(count * price)
    gainCurrency(currency, count)
    this
  }

  /**
    * Operation, that allows client to filter the list of currencies.
    */
  def filterCurrencies(newCurs: TraversableOnce[String]) = {
    currencies ++= newCurs.filter(c => !currencies.contains(c)).map(_ -> 0)
    currencies --= currencies.keySet -- newCurs
    this
  }
  
  /**
    * Operation that adds a currency to a list of supported currencies.
    */
  def addCurrency(currency : String, startCount : Int = 0)= {
    currencies += currency -> startCount
    this
  }
  
  /**
    * Operation that removes a currency from a list of supported currencies.
    */
  def removeCurrency(currency : String, price : Double = 1.0) = {
    balance += currencies(currency) * price
    currencies -= currency
    this
  }
  
  /**
    * Utility function that updates the balance.  
    */
  def updateBalance(money: Double) = balance = money
  
  /**
    * Utility function that updates the currency.  
    */
  def updateCurrency(currency: String, count: Int) = currencies(currency) = count
  
  /**
    * Utility function that adds the given money to a current balance.
    */
  def gainBalance(money: Double) = balance += money
  
  /**
    * Utility function that updates the balance.  
    */
  def wasteBalance(money: Double) = balance -= money
  
  /**
    * Utility function that updates the currencies with a given.  
    */
  def gainCurrency(currency: String, count: Int) = currencies.update(currency, currencies(currency) + count)
  
  /**
    * Utility function that removes a currency.
    */
  def wasteCurrency(currency: String, count: Int) = currencies.update(currency, currencies(currency) - count)
  
  /**
    * Determines, if a client can spend the money. 
    */
  def canSpend(money: Double): Boolean = balance >= money
  
  /**
    * Determines, if a client can sell the currency. 
    */
  def canSell(currency: String, count: Int): Boolean = currencies(currency) >= count

  /**
    * Gets a given currency
    */
  def get(currency: String): Int = currencies(currency)

  override def toString: String = s"$name: $balance$$ | $currencies"
}
