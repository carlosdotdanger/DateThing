package net.tummi


class OutofBounds(value: Int, field: String)  extends Exception{
	override def toString = "%d is out of bounds for %s".format(value,field)
}



object DateMaths{
	final val M_Nums =   Array(0,3,3,6,1,4,6,2,5,0,3,5)
	final val M_NumsL = Array(-1,2,3,6,1,4,6,2,5,0,3,5) 
	final val MONTH_DAYS = Array(31,28,31,30,31,30,31,31,30,31,30,31)
	final val ordDays = Array(0,31,59,90,120,151,181,212,243,273,304,334)
	final val ordDaysL = Array(0,31,60,91,121,152,182,213,244,274,305,335)

	def dayOfYear(d: Date): Int = {
		if (isLeap(d.y)) ordDaysL((d.m - 1).asInstanceOf[Int]) + d.d
		else ordDays((d.m - 1).asInstanceOf[Int]) + d.d	
	}

	def daysLeftinYear(d:Date): Int = {
		if(isLeap(d.y)) 366 - dayOfYear(d) 
		else 365 - dayOfYear(d) 
	}

	def wkDay(d:Date): Int ={
		(d.d + mNum(d) + (d.y % 100) + ((d.y % 100)/4) + cNum(d.y)) % 7 
	}

	def cWeek(d: Date): Cweek ={ 
		val w = (dayOfYear(d) - wkDay(d) + 11) / 7
		if(w > 51 && d.m == 1)  Cweek(d.y - 1, w)
		else Cweek(d.y,w)
	}

	def isLeap(y: Int): Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	
	def leapsBetween(d1: Date, d2: Date): Int = leapsBetween(d1.y,d2.y)
	def leapsBetween(y1: Int,y2: Int): Int ={
		val (l,g) = if(y1 > y2) (y2 + 1, y1 -1) else (y1 + 1, y2 -1)
		leaps(g) - leaps(l)
	}
	
	def every(dy: Days = (1 :: Days), d: Date = DAY_ZERO, until: Date => Boolean = {x => false}): Stream[Date] ={ 
		val nd = d + dy
		if(!until(nd)){ d #:: every(dy, nd, until)}
		else d #:: Stream.empty 
	}
	//internal 
	def leaps(y: Int): Int = (math.floor(y / 4) - math.floor(y / 100) + math.floor(y / 400)).asInstanceOf[Int]	

	def chkDate(dt: Date): Boolean = {
		dt.y match{
			case by if (by > 99999  || by < 0) => false
			case y =>
				dt.m match {
					case bm if(bm > 12 || bm < 1) => false
					case m =>
						dt.d match {
							case bd if(bd < 0) => false 
							case d => 
								if(m == 2 && d == 29) isLeap(y) 
								else MONTH_DAYS(m - 1) >= d
						}
						 
				}
				
		}
	}
	def mNum(d: Date): Int ={
		if(isLeap(d.y)) M_NumsL(d.m - 1)
		else M_Nums(d.m - 1)	
	}
	
	def cNum(y: Int): Int = 6 - ((((y - (y % 100))/100) % 4) * 2)  	

	def toDays(d:Date):Long = {
		val m = (d.m + 9) % 12
		val y = d.y - m/10
		lf(y) + ((m * 306) + 5)/10 + (d.d -1)
	}
	
	def toDate(x: Long): Date ={
		var y = (10000L * x + 14780L)/3652425L	
		var ddd = x - lf(y)
		if(ddd < 0){
			y = y - 1
			ddd = x - lf(y) 
		}
		val mi = (100L * ddd + 52)/3060L
		val mm = (mi + 2) % 12 + 1
		y = y + (mi + 2)/12
		val dd = ddd - ((mi*306) + 5)/10 + 1
		Date(y.asInstanceOf[Int], mm.asInstanceOf[Int], dd.asInstanceOf[Int])
	}
	
	def lf(y: Long) = (365*y) + y/4 - y/100 + y/400

}

