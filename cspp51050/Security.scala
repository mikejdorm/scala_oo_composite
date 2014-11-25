package cspp51050

/**
 * Security leaf implementation
 */
sealed trait Security extends Leaf {
  val description: String
  val symbol: String
  val isin: String
  var price: Double = 1.00d
}
/**
 * Bond security
 */
case class Bond(val description: String, val symbol: String,
  val isin: String) extends Security {
  var interestRate:Double = InterestRates.corporateRate
  var faceValue:Double = 1000d
  override def getName(): String = "Bond: " + isin + " " + symbol
  override def accept(visitor: PricingVisitor) = {
    visitor.visitBond(this)
  }
}
/**
 * Money market security
 */
case class MoneyMarket(val description: String, val symbol: String,
  val isin: String) extends Security {
  price = InterestRates.moneyMarketRate
  override def accept(visitor: PricingVisitor) = {
    visitor.visitMoneyMarket(this)
  }
  override def getName(): String = "Money Market Fund: " + isin + " " + symbol
}
/**
 * Stock security implementation
 */
case class Stock(val description: String, val symbol: String,
  val isin: String) extends Security {
  val dividend = 1.00d
  override def accept(visitor: PricingVisitor) = {
    visitor.visitStock(this)
  }
  override def getName(): String = "Equity: " + isin + " " + symbol
}
