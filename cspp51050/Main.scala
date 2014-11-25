package cspp51050


object Main {
  def main(args: Array[String]):Unit = { 
		 // TestAccountCreation
		  TestPricing
  }
}

/*
 * Test the creation of accounts and portfolios
 */
object TestAccountCreation{

	val manager = PortfolioManager
	manager.createAccount(1,"MyFirstAccount")
	manager.createPortfolio(2, "MyFirstPortfolio")
	manager.createSecurity(2, "Some Bond", "BD", "BND1")
    manager.printAllAccounts
    manager.printAllPortfolios
    manager.printAllSecurities

}
/*
 * test the pricing of securities.
 */
object TestPricing{
	val manager = PortfolioManager
	manager.createAccount(1,"MyFirstAccount")
	manager.createPortfolio(2, "MyFirstPortfolio")
	manager.createSecurity(2, "Some Bond", "BD", "BND1")
	manager.createSecurity(1, "Google", "GOOG", "123")
	manager.securities("123").price = 399.99
    manager.printAllAccounts
    manager.printAllPortfolios
    manager.printAllSecurities
    val portfolio = manager.portfolios.values.head
    portfolio.add(new SecurityPosition(10, manager.securities("BND1")))
    portfolio.add(new SecurityPosition(100, manager.securities("123")))
    prettyPrint.dollarValue(portfolio.getName, manager.evaluatePortfolio(portfolio.id))
    val account = manager.accounts.values.head
    account.add(new SecurityPosition(19, manager.securities("123")))
    prettyPrint.dollarValue(account.getName, manager.evaluateAccount(account.accountNumber))
 
}

/*
 * Print dollar amounts. 
 */
object prettyPrint{
	def dollarValue(name:String, value:Double):Unit = println(name+" is worth $%.2f".format(value))
}
