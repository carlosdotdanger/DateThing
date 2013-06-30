package net.tummi


object DateMaths{
	final val M_Nums =   Array(0,3,3,6,1,4,6,2,5,0,3,5)
	final val M_NumsL = Array(-1,2,3,6,1,4,6,2,5,0,3,5) 
	final val MONTH_DAYS = Array(31,28,31,30,31,30,31,31,30,31,30,31)
	final val ordDays = Array(0,31,59,90,120,151,181,212,243,273,304,334)
	final val ordDaysL = Array(0,31,60,91,121,152,182,213,244,274,305,335)

	def monthDays(y:Int,m: Int): Int ={
		m match{
			case 2 => if(isLeap(y)) 29 else 28
			case x => MONTH_DAYS(x-1)
		}
	}

	def dayOfYear(d: Date): Int = {
		if (isLeap(d.y)) ordDaysL((d.m - 1).asInstanceOf[Int]) + d.d
		else ordDays((d.m - 1).asInstanceOf[Int]) + d.d	
	}

	def wkDay(d:Date): Int ={
		((d.d + mNum(d) + (d.y % 100) + ((d.y % 100)/4) + cNum(d.y)) - 1) % 7 
	}

	def week(d: Date): Week ={ 
		val nd = toDate( toDays(d) + (3 - wkDay(d)))
		val w = (dayOfYear(nd) + 6) / 7
		Week(nd.y,w)
	}
    
	def dateFromWeek(w: Week, day: Int = 0): Date ={
		val j1 = Date(w.y, 1, 1)
		val j1WkDay = wkDay(j1)
		val j1Wk: Week = j1
		val firstMon = j1Wk.y match{
			case w.y => j1 - (j1WkDay :: Days)
			case _: Int => j1 + ((7 - j1WkDay ) :: Days)
		}
		firstMon + ( (7 * (w.w - 1)) + day :: Days)
	}

	def isLeap(y: Int): Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	
	
	//internal 
	def leaps(y: Int): Int = (math.floor(y / 4) - math.floor(y / 100) + math.floor(y / 400)).asInstanceOf[Int]	

	def chkDate(y: Int,m: Int, d: Int): Boolean = {
		y match{
			case by if (by > 9999  || by < 0) => false
			case gy =>
				m match {
					case bm if(bm > 12 || bm < 1) => false
					case gm =>
						d match {
							case bd if(bd < 1) => false 
							case gd => 
								if(gm == 2 && gd == 29) isLeap(gy) 
								else MONTH_DAYS(gm - 1) >= gd
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

