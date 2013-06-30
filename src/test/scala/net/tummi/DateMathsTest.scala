package net.tummi 

import org.scalatest.FunSuite
import net.tummi.DateMaths._

class DateMathsSuite extends FunSuite{
	val fri = Date(2013,5,24)
	val lyear = Date(2000,2,29)

	test("wkDay"){
		//jan 1st 1970 is thursday
		val i = Date.every(from = Date(1970,1,1)).take(1000000).iterator
		var wday = 3
		for (d <- i){
			assert(wkDay(d) === wday)
			wday = (wday + 1) % 7
		}
	}
	
	test("cnum"){
		assert(cNum(2000) === 6)
		assert(cNum(1610) === 6)
		assert(cNum(2089) === 6)
		assert(cNum(2111) === 4)
		assert(cNum(2100) === 4)
		assert(cNum(1776) === 4)
		assert(cNum(1987) === 0)
		assert(cNum(1950) === 0)
		assert(cNum(1550) === 0)
		assert(cNum(1450) === 2)
		assert(cNum(1850) === 2)
		assert(cNum(2289) === 2)		
	}

	test("mnum"){
		assert(mNum(fri) == 1)
		assert(mNum(lyear) == 2)
	}
	
	test("isLeap"){
		assert(isLeap(2100) === false )
		assert(isLeap(3000) === false )
		assert(isLeap(2000) === true )
		assert(isLeap(2008) === true )
		assert(isLeap(1999) === false )

	}  

	test("chkDate"){
		assert(chkDate(10000,1,1) === false)
		assert(chkDate(-1,1,1) === false)
		assert(chkDate(2000,-1,1) === false)
		assert(chkDate(2000,13,1) === false)
		assert(chkDate(2000,1, -1) === false)
		assert(chkDate(2001,1,50) === false)
		assert(chkDate(2001,2,29) === false)
	}


	test("day of year"){
		assert(dayOfYear(Date(2000,1,1)) === 1)
		assert(dayOfYear(Date(2000,12,31)) === 366)
		assert(dayOfYear(Date(2001,1,1)) === 1)
		assert(dayOfYear(Date(2001,12,31)) === 365)
	}


	test("week"){
		assert(week(Date(2013,6,26)) === Week(2013,26))
		assert(week(Date(2013,6,24)) === Week(2013,26))
		assert(week(Date(2013,1,1)) === Week(2013,1))		
		assert(week(Date(2012,12,31)) === Week(2013,1))
		assert(week(Date(2009,12,27)) === Week(2009,52))			
		assert(week(Date(2009,12,28)) === Week(2009,53))
		assert(week(Date(2009,12,29)) === Week(2009,53))			
		assert(week(Date(2009,12,30)) === Week(2009,53))	
		assert(week(Date(2009,12,31)) === Week(2009,53))			
		assert(week(Date(2010,1,1)) === Week(2009,53))
		assert(week(Date(2010,1,2)) === Week(2009,53))
		assert(week(Date(2010,1,3)) === Week(2009,53))
		assert(week(Date(2010,1,4)) === Week(2010,1))		

	}

}

