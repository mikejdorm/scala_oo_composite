package cspp51050
import scala.collection.mutable.HashMap

//Composite map is an extension of HashMap that holds a generic composite type.
class CompositeMap[V <: Composite] extends HashMap[Int, V]

object PortfolioManager {
 
  val portfolios = new CompositeMap[Portfolio[_ <: Component]]
  val accounts = new CompositeMap[Account[_ <: Component]]
  val securities = new HashMap[String, Security]
  var nextPrivateAccountNumber = 1000
  var nextFirmAccountNumber = 1
  var nextInstiutionalAccountNumber = 2000;
  var nextPortfolioId = 1

  /**
   * Create account based on enum account type values. 
   */
  def createAccount(acctType: Int, name: String) = acctType match {
    case AccountType.FirmAccount => {
      accounts.put(nextFirmAccountNumber, new FirmAccount(nextFirmAccountNumber, name))
      nextFirmAccountNumber = nextFirmAccountNumber + 1
    }
    case AccountType.InstitutionalAccount => {
      accounts.put(nextInstiutionalAccountNumber, new InstitutionalAccount(nextInstiutionalAccountNumber, name))
      nextInstiutionalAccountNumber = nextInstiutionalAccountNumber + 1
    }
    case AccountType.PrivateAccount => {
      accounts.put(nextPrivateAccountNumber, new PrivateAccount(nextPrivateAccountNumber, name))
      nextPrivateAccountNumber = nextPrivateAccountNumber + 1
    }
    case _ => println("Not sure what that is!!!")
  }
  
  /**
   * prints all the accounts held in the account hashmap
   */
  def printAllAccounts() = {
    this.accounts.keys.foreach{i =>  
                           print( "Key = " + i )
                           println(" Value = " + accounts(i).getName )
    }
  }
  /**
   * print all the accounts held in the portfolio hashmap
   */
  def printAllPortfolios() = {
      this.portfolios.keys.foreach{i =>  
                           print( "Key = " + i )
                           println(" Value = " + portfolios(i).getName )
    }
  }
  /**
   * print all the available securities held in the security hashmap
   */
  def printAllSecurities() = {
      this.securities.keys.foreach{i =>  
                           print( "Key = " + i )
                           println(" Value = " + securities(i).getName )
    }
  }
  
  /**
   * create a portfolio object based on the portfolio type enum value
   */
  def createPortfolio(portfolioType: Int, name: String) = portfolioType match {
    case PortfolioType.InstitutionalPortfolio => {
      portfolios.put(nextPortfolioId, new InstitutionalPortfolio(nextPortfolioId, name))
      nextPortfolioId = nextPortfolioId + 1
    }
    case PortfolioType.BrokerPortfolio => {
      portfolios.put(nextPortfolioId, new BrokerPortfolio(nextPortfolioId, name))
      nextPortfolioId = nextPortfolioId + 1
    }
    case PortfolioType.ClientPortfolio => {
      portfolios.put(nextPortfolioId, new ClientPortfolio(nextPortfolioId, name))
      nextPortfolioId = nextPortfolioId + 1
    }
    case _ => println("Not sure what that is!!!")
  }

  /**
   * Create security object based on security type enum, name, symbol, and isin. The
   * isin is the hashmap key given that it's considered to be unique. 
   */
  def createSecurity(secType: Int, name: String, symbol: String, isin: String) = secType match {
    case SecurityType.Stock => securities.put(isin, new Stock(name, symbol, isin))
    case SecurityType.Bond => securities.put(isin, new Bond(name, symbol, isin))
    case SecurityType.MoneyMarket => securities.put(isin, new MoneyMarket(name, symbol, isin))
    case _ => println("Not sure what that is!!!")
  }

  /**
   * Get the value of an account
   */
  def evaluateAccount(acctNumber: Int):Double = {
      val visitor:PricingVisitor = new PricingVisitor()
      val account = accounts(acctNumber)
      println("Evaluating " + account.getName)
      visitor.visitAccount(account)
      visitor.value
  }
  /**
   * get the value of a portfolio
   */
  def evaluatePortfolio(portfolioId: Int):Double = {
  	  val visitor:PricingVisitor = new PricingVisitor()
  	  val portfolio = portfolios(portfolioId)
  	  println("Evaluating " + portfolio.getName)
      visitor.visitPortfolio(portfolio)
      visitor.value
  }

}

/**
 * Interest rate values.
 */
object InterestRates{
  
  val corporateRate = 0.08
  val muniRate = 0.07
  val privRate = 0.10
  val moneyMarketRate = 0.01
  
  def updateRate(rate:Double, rateType:Int) = rateType match{
    case 1 => val corporateRate = rate
    case 2 => val muniRate = rate
    case 3 => val privRate = rate
    case _  => println("Rate not supported")
  }
}

// Enumerations for portfolio, account, and security types.
object PortfolioType extends Enumeration {
  val InstitutionalPortfolio = 1
  val BrokerPortfolio = 2
  val ClientPortfolio = 3
}
object SecurityType extends Enumeration {
  val Stock = 1
  val Bond = 2
  val MoneyMarket = 3
}

object AccountType extends Enumeration {
  val PrivateAccount = 1
  val InstitutionalAccount = 2
  val FirmAccount = 3
}



