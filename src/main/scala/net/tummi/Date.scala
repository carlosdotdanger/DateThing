package net.tummi

//import DateImplicits._
import scala.language.implicitConversions
import net.tummi.Date._



case class BadDateException(y: Int,m: Int, d:Int) extends Exception() 

sealed trait DateObject

case class Cweek(y: Int, w: Int) extends DateObject with Ordered[Cweek]{
	override def compare(o: Cweek): Int = {
		y - o.y match{
			case 0 => w - o.w
			case d => d
		}
	}
}

case class Month(y: Int, m: Int) extends DateObject with Ordered[Month]{
	override def compare(o: Month): Int = {
		y - o.y match{
			case 0 => m - o.m
			case d => d
		}
	}
}

case class Year(y: Int) extends DateObject with Ordered[Year]{
	override def compare(o: Year): Int = y - o.y
}


case class Date(val y: Int, val m: Int, val d: Int) extends Ordered[Date]{
	if(! DateMaths.chkDate(this))
		throw BadDateException(y,m,d)
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


object Date{
	implicit def Date2Month(d: Date): Month = Month(d.y, d.m)
	implicit def Date2Year(d: Date): Year = Year(d.y)
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