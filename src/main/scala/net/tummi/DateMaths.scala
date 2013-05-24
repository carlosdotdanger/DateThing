package net.tummi

import scala.math

class OutofBounds(value: Int, field: String)  extends Exception{
	override def toString = "%d is out of bounds for %s".format(value,field)
}

object DateMaths{
	final val MNums = Array(0,3,3,6,1,4,6,2,5,0,3,5)
	final val leapMNums = Array(-1,2,3,6,1,4,6,2,5,0,3,5) 
	
	def wkDay(d:Date): Int ={
		(d.d + mNum(d.m) + (d.y % 100) + ((d.y % 100)/4) + cNum(d.y)) % 7 
	}
	
	def isLeap(y: Int): Boolean = (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0 )
	def leapsBetween(year1:Int,year2:Int): Int ={
		val y1 = year1 - 1
		val y2 = year2 - 1
		val l1 =( math.floor(y1 / 4) -  math.floor (y1 / 100) + math.floor (y1 / 400)).asInstanceOf[Int]	
		val l2 =(math.floor(y2 / 4) - math.floor(y2 / 100) + math.floor(y2 / 400)).asInstanceOf[Int]	
		math abs (l1 - l2)
	}
	
	def mNum(m: Int): Int ={
		if(isLeap(m)) leapMNums(m)
		else MNums(m)	
	}
	
	def cNum(y: Int): Int = 6 - ((((y - (y % 100))/100) % 4) * 2)  	
		

	private def bc[A](f: Int => A, a: Int, l: Int, u: Int,t: String){
		if(a < 1 || a > 12) throw new OutofBounds(a,"month")
		f(a)
	}
}

