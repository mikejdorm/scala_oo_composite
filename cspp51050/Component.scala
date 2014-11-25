package cspp51050
import scala.collection.immutable.List
import java.util.{ Date, Locale }

/**
 * Component trait acting as an interface for components.
 */
trait Component {
  def accept(visitor: PricingVisitor)
  def getName(): String
}

/**
 * leaf trait for the leaf classes of the pattern
 */
trait Leaf extends Component

/**
 * SecurityPosition holds a security object and the 
 * quantity held by the portfolio or account
 */
class SecurityPosition(val quantity: Int, val security: Security) extends Leaf {
  val key = security.isin
  override def accept(visitor: PricingVisitor) = {
     visitor.visitSecurityPosition(this)
  }
  override def getName: String = "Position: " + quantity + " => " + security.getName()
}

/**
 * Composite trait acting as an abstract class for the composite classes
 * Account and Portfolio
 */
trait Composite extends Component {
  var components: List[Component] = List()
  def add(component: Component): Unit = {
    println("Adding " + component.getName +" to: " + this.getName)
    this.components = component::this.components
    println("Components: " + this.components)
  }
  def delete(component: Component): Unit = {
    this.components = for (y <- this.components if (component != y)) yield y
  }
}
/**
 * Account trait. Extension of composite. The account trait
 * can be initialized to hold any type of component so it can
 * hold a series of accounts, portfolios, or security positions.
 */
sealed trait Account[X <: Component] extends Composite {
  val accountNumber: Int
  val name: String
  val openDate: Date = new Date
  var value:Double = 0d
 override def accept(visitor: PricingVisitor) = {
     println("Visiting Account Class")
    visitor.visitComposite(this)
    this.value = visitor.value
  }
/*
  override def valuate(): Double = {
    this.components.map(_.valuate).sum
  }*/
  override def getName: String = "Account " + accountNumber.toString + ": " + name
}
/**
 * PrivateAccount implementation of account. A private account holds security positions
 * and is meant for retail brokerage. 
 */
case class PrivateAccount(val accountNumber: Int, val name: String) extends Account[SecurityPosition] {
  override def getName: String = "PrivateAccount: " + accountNumber.toString + " : " + name
}
/**
 * An institutional account holds a series of Private Accounts that then hold security positions.
 */
case class InstitutionalAccount(val accountNumber: Int, val name: String) extends Account[PrivateAccount] {
  override def getName: String = "InstitutionalAccount: " + accountNumber.toString + " : " + name
}

/**
 * A Firm account holds a series of different accounts. 
 */
case class FirmAccount(val accountNumber: Int, val name: String) extends Account[Component] {
  override def getName: String = "FirmAccount: " + accountNumber.toString + " : " + name
}

/**
 * Portfolio trait acting as an abstract class for portfolio implementations
 */
trait Portfolio[X <: Component] extends Composite {
  val name: String
  val id: Int
  var iterator: PortfolioIterator[Portfolio[X]] = new PortfolioIterator(this)
  var value:Double = 0d
  
 override def accept(visitor: PricingVisitor) = {
    this.iterator = new PortfolioIterator(this)
     println("Visiting Portfolio Class")
    visitor.visitPortfolio(this)
    this.value = visitor.value
  }
/*
  override def valuate(): Double = {
    var sum: Double = 0
    while (iterator.hasNext){
       sum = sum + iterator.next.valuate
    }
    sum
  }
  * 
  */
  def getName(): String = "Portofolio " + this.name

}
/**
 * Institutional portfolio holds a series of institiutional accounts.
 */
case class InstitutionalPortfolio(val id: Int, val name: String) extends Portfolio[InstitutionalAccount] {
  override def getName: String = "InstitutionalAccount: " + name
}
/**
 * Broker portfolio holds a series of client portfolios.
 */
case class BrokerPortfolio(val id: Int, val name: String) extends Portfolio[ClientPortfolio] {
  override def getName: String = "InstitutionalAccount: " + name
}
/**
 * Client Porfolio holds a series of private accounts. Meant as a way for one client
 * to keep all accounts under one umbrella. 
 */
case class ClientPortfolio(val id: Int, val name: String) extends Portfolio[PrivateAccount] {
  override def getName: String = "InstitutionalAccount: " + name
}