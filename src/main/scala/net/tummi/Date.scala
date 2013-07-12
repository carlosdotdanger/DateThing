package net.tummi
import DateMaths._
import scala.language.implicitConversions
//import net.tummi.Date._




case class BadDateException(y: Int,m: Int, d:Int) extends Exception() 

sealed abstract trait DateObject{
	def start: Date
	def end: Date
}

case class Week(y: Int, w: Int) extends DateObject with Ordered[Week]{
	def start: Date = dateFromWeek(this)
	def end: Date = dateFromWeek(this,6)
	override def compare(o: Week): Int = {
		y - o.y match{
			case 0 => w - o.w
			case d => d
		}
	}

	def + (wks: Weeks): Week = week(dateFromWeek(this) + ((wks.n * 7) :: Days) )
	override def toString = "%04d-%02d".format(y,w)
}

object Week{
	implicit def Week2Year(c: Week) = new Year(c.y)
	
	def every(	dw: Weeks = (1 :: Weeks), from: Week = DAY_ZERO, 
				until: Week => Boolean = {x => false}): Stream[Week] ={ 
		val nd = from + dw
		if(!until(nd)){ from #:: every(dw, nd, until)}
		else from #:: Stream.empty 
	}
}

case class Month(y: Int, m: Int) extends DateObject with Ordered[Month]{
	if(!chkDate(y,m,1))
		throw new BadDateException(y,m,1)
	def start: Date =  new Date(y,m,1)
	def end: Date = new Date(y,m,monthDays(y,m))
	override def compare(o: Month): Int = {
		y - o.y match{
			case 0 => m - o.m
			case d => d
		}
	}

	def + (mts: Months): Month = {
		if(mts.n < 0)  return this - (mts.n * - 1 :: Months) 
		val nmod = mts.n % 12
		val nmodtot = nmod + m
		if(nmodtot > 12) new Month(y + 1 + (mts.n)/12 , nmodtot - 12) 
		else new Month (y + (mts.n)/12, nmodtot)
		
	}
	def - (mts: Months): Month = {
		if(mts.n < 0) return this + (mts.n * - 1 :: Months) 
		val nmod = mts.n % 12
		val nmodtot = m - nmod
		if(nmodtot < 1) new Month( y - 1 - mts.n/12, nmodtot + 12)
		else new Month(y - mts.n/12, nmodtot)
			
	}
	def + (yrs: Years): Month = new Month(y + yrs.n, m)
	def - (yrs: Years): Month = new Month(y - yrs.n, m)
	override def toString = "%04d-%02d".format(y,m)
}

object Month{
	implicit def Month2Year(m:Month): Year = new Year(m.y)
	def every(	dy: Months = (1 :: Months), from: Month = DAY_ZERO, 
				until: Month => Boolean = {x => false}): Stream[Month] ={ 
		val nd = from + dy
		if(!until(nd)){ from #:: every(dy, nd, until)}
		else from #:: Stream.empty 
	}	

}

case class Year(y: Int) extends DateObject with Ordered[Year]{
	if(!chkDate(y,1,1))
		throw new BadDateException(y,1,1)
	def start: Date =  new Date(y,1,1)
	def end: Date = new Date(y,12,31)
	override def compare(o: Year): Int = y - o.y
	def + (yrs: Years) = new Year(y + yrs.n)
	def - (yrs: Years) = new Year(y - yrs.n)
	override def toString = y.toString
}

object Year{
	def every(dy: Years = (1 :: Years), from: Year = DAY_ZERO, until: Year => Boolean = {x => false}): Stream[Year] ={ 
		val nd = from + dy
		if(!until(nd)){ from #:: every(dy, nd, until)}
		else from #:: Stream.empty 
	}
}

case class Date(val y: Int, val m: Int, val d: Int) extends Ordered[Date]{
	if(! chkDate(y,m,d))
		throw BadDateException(y,m,d)
	lazy val day: Int = wkDay(this)
	def start: Date =  new Date(y,m,d)
	def end: Date = new Date(y,m,d)
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
	
	def + (dys: Days): Date = 
		dys.n match{
			case 0 => new Date(y,m,d)
			case n => toDate(toDays(this) + n) 
		}
	def - (dys: Days): Date = 
		dys.n match{
			case 0 => new Date(y,m,d)
			case n => toDate(toDays(this) - n) 
		}

	override def toString = "%04d-%02d-%02d".format(y,m,d)
	
}	

object Date{

	implicit def Date2Month(d: Date): Month = Month(d.y, d.m)
	implicit def Date2Year(d: Date): Year = Year(d.y)
	implicit def Date2Week(d: Date): Week = week(d)

	def apply(s: String): Option[Date] = {
		try{
			val parts = s.split("-")
			Some(new Date(parts(0).toInt,parts(1).toInt,parts(2).toInt))
		}catch{
			case _: Throwable => None
		}
	}

	def every(dy: Days = (1 :: Days), from: Date = DAY_ZERO, until: Date => Boolean = {x => false}): Stream[Date] ={ 
		val nd = from + dy
		if(!until(nd)){ from #:: every(dy, nd, until)}
		else from #:: Stream.empty 
	}
}

object DAY_ZERO extends Date(0,3,1)

object END_OF_DAYS extends Date(9999,12,31) 

object Today{
	def apply(): Date = FromMillis(System.currentTimeMillis)
}

object FromMillis{
	def apply(ts: Long): Date = {
		val d = (ts/86400000).asInstanceOf[Int]
		Date(1970,1,1) + (d :: Days)
	}
}