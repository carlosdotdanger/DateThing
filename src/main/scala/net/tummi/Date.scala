package net.tummi


case class Date(y: Int, m: Int, d: Int) extends Ordered[Date]{
	lazy val isLeap: Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	
	override def compare(o: Date): Int ={
		y - o.y match{
			case 0 => 
				m - o.m match{
					case 0 => d - o.d 
					case md => md
				} 
			case yd => yd
		}
	}

	//comparisons
	def <=> (o: Date): Int = this.compare(o)	
	override def < (o:Date): Boolean = this.compare(o) < 0
	override def > (o:Date): Boolean = this.compare(o) > 0
	override def <= (o:Date): Boolean = this.compare(o) < 1
	override def >= (o:Date): Boolean = this.compare(o) > -1
	def == (o: Date): Boolean = this.compare(o) == 0
	
	//maths
	//TODO: obviously this isn't right, just for testing
	def + (dys: Days): Date = new Date(y,m, d + dys.n)
	def + (mts: Months): Date = new Date(y,m + mts.n,d)	
	def + (yrs: Years): Date = new Date(y + yrs.n,m, d)
	override def toString = "%02d-%02d-%02d".format(y,m,d)
	
}	

