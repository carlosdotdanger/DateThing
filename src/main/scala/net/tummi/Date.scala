package net.tummi


case class Date(val y: Int, val m: Int, val d: Int) extends Ordered[Date]{
	lazy val isLeap: Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	override def compare(o: Date): Int ={
		y - o.y match{
			case 0 => 
				m - o.m match{
					case 0 => (d - o.d)
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
	
	def + (dys: Days): Date = 
		dys.n match{
			case 0 => new Date(y,m,d)
			case n => DateMaths.toDate(DateMaths.toDays(this) + n) 
		}
	def - (dys: Days) = 
		dys.n match{
			case 0 => new Date(y,m,d)
			case n => DateMaths.toDate(DateMaths.toDays(this) - n) 
		}
	def + (mts: Months): Date = {
		val dm = m + mts.n % 12
		val (ey,nm) = if(dm > 12) (1, (m + mts.n) % 12) else (0,dm) 
		new Date(y + mts.n/12 + ey, nm, d)
	}
	def - (mts: Months): Date = {
		val dm = m - mts.n % 12
		val (ey,nm) = if(dm < 1) (1, (m - mts.n) % 12) else (0,dm) 
		new Date(y - mts.n/12 - ey, nm, d)
	}	
	
	def + (yrs: Years): Date = new Date(y + yrs.n, m, d)
	def - (yrs: Years): Date = new Date(y - yrs.n, m, d)

	override def toString = "%04d-%02d-%02d".format(y,m,d)
	
}	
