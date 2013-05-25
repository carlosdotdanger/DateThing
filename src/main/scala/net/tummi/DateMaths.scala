package net.tummi


class OutofBounds(value: Int, field: String)  extends Exception{
	override def toString = "%d is out of bounds for %s".format(value,field)
}

object DateMaths{
	final val M_Nums = Array(0,3,3,6,1,4,6,2,5,0,3,5)
	final val LM_Nums = Array(-1,2,3,6,1,4,6,2,5,0,3,5) 
	
	def wkDay(d:Date): Int ={
		(d.d + mNum(d) + (d.y % 100) + ((d.y % 100)/4) + cNum(d.y)) % 7 
	}
	
	def isLeap(y: Int): Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	
	def leapsBetween(d1: Date, d2: Date): Int = leapsBetween(d1.y,d2.y)
	def leapsBetween(year1:Int,year2:Int): Int ={
		val (y1,y2) = if(year1 > year2) (year2 + 1, year1 -1) else (year1 + 1, year2 -1)
		val l1 =(math.floor(y1 / 4) - math.floor(y1 / 100) + math.floor(y1 / 400)).asInstanceOf[Int]	
		val l2 =(math.floor(y2 / 4) - math.floor(y2 / 100) + math.floor(y2 / 400)).asInstanceOf[Int]	
		l2 - l1
	}
	
	def mNum(d: Date): Int ={
		if(isLeap(d.y)) LM_Nums(d.m - 1)
		else M_Nums(d.m - 1)	
	}
	
	def cNum(y: Int): Int = 6 - ((((y - (y % 100))/100) % 4) * 2)  	
		
	private def bc[A](f: Int => A, a: Int, l: Int, u: Int,t: String){
		if(a < 1 || a > 12) throw new OutofBounds(a,t)
		f(a)
	}
}

