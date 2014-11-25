package cspp51050


class PricingVisitor{

  /**
   * var to hold the aggregated security values
   */
    var value:Double = 0d
    
	def visitBond(bond:Bond) = {
	    value = value + (bond.faceValue + (bond.faceValue*bond.interestRate))
	}
	def visitStock(stock:Stock) = {
	  	 value = value + stock.price + stock.dividend
	}
	def visitMoneyMarketPosition(mm:MoneyMarket, quantity:Int) = {
	    value = value + (1+mm.price)*quantity
	}
	def visitMoneyMarket(mm:MoneyMarket) = {
	    value = value + (1+mm.price)
	}
	
	/**
	 * visit security position iterates over the positions.
	 */
	def visitSecurityPosition(pos:SecurityPosition):Unit = pos.security match{
	  case s: Stock =>   { value = ((s.price + s.dividend) * pos.quantity) + value
			  				println("Value: " + this.value)}
	  case b: Bond =>  {value = ((b.faceValue + (b.faceValue*b.interestRate)) * pos.quantity) + value
	  						println("Value: " + this.value)}
	  case m: MoneyMarket => {value = ((1+m.price)*pos.quantity) + value
	  						println("Value: " + this.value)}
	  case _ => println("Not sure what security you got there!")
	}
	
	/**
	 * Visits composite takes a generic composite object.
	 */
	def visitComposite[X <: Component](composite:X):Unit = composite match{
	  case p: InstitutionalPortfolio =>  while(p.iterator.hasNext){ p.iterator.next.accept(this) }
	  case p: BrokerPortfolio => while(p.iterator.hasNext){ p.iterator.next.accept(this) }
	  case p: ClientPortfolio =>  while(p.iterator.hasNext){p.iterator.next.accept(this) }
	  case p: PrivateAccount => p.components.foreach(i => i.accept(this))
	  case p: InstitutionalAccount => p.components.foreach(i => i.accept(this))
	  case p: FirmAccount => p.components.foreach(i => i.accept(this))
	  case _ => print("Not sure what your visiting... " )
	}
	/**
	 * Visits account object
	 */
	def visitAccount[X <: Component](account:Account[_ <: Component]):Unit = account match{
	  case a: PrivateAccount => a.components.foreach(i => i.accept(this))
	  case a: InstitutionalAccount => a.components.foreach(i => i.accept(this))
	  case a: FirmAccount => a.components.foreach(i => i.accept(this))
	  case _ => print("Not sure what your visiting... " )
	}
	 /**
	  * visit portfolio object
	  */
	def visitPortfolio[X <: Component](portfolio:Portfolio[_ <: Component]):Unit = portfolio match{
	  case p: InstitutionalPortfolio => while(p.iterator.hasNext){ p.iterator.next.accept(this) }
	  case p: BrokerPortfolio => while(p.iterator.hasNext){ p.iterator.next.accept(this) }
	  case p: ClientPortfolio => while(p.iterator.hasNext){p.iterator.next.accept(this) }
	  case _ => print("Not sure what your visiting... " )
	}

}