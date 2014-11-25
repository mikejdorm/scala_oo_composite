package cspp51050

class PortfolioIterator[X <: Portfolio[_ <: Component]](val portfolio:X) {

   protected var _current:Int = -1
/**
 * get first component or none if list is empty
 */
   def first():Option[Component] = {
     if(!this.portfolio.components.isEmpty)
    	 Option(this.portfolio.components.head) 
     else 
    	 None
   }
   
   	/**
	 * increment iterator to next component
	 */
   def next():Component = {
     println("Moving to next component of: " + portfolio.getName)
		if(hasNext){
		   _current = _current+1
		   this.portfolio.components(_current)
		}
		else{
		  this.portfolio.components(_current)
		}
   }
     /**
      * return true if there is another component in the list
      */
   def hasNext():Boolean = {
      if(_current+1<portfolio.components.size){
        println("Portfolio has another value" + portfolio.getName)
         true
      }
      else{
        println("Portfolio doesn't have another value" + portfolio.getName)
        false
      }
   }
   
   /**
    * get current component
    */
   def current():Component = current
   
   /**
    * get last component.
    */
   def last():Component = this.portfolio.components.last
   
}	