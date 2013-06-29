package net.tummi 

import org.scalatest.FunSuite

class DateMathsSuite extends FunSuite{
	val fri = Date(2013,5,24)
	val lyear = Date(2000,2,29)

	test("wkDay"){
		assert(DateMaths.wkDay(fri) === 5)
		assert(DateMaths.wkDay(lyear) === 2)
		assert(DateMaths.wkDay(Date(1986,6,1)) === 0)
		assert(DateMaths.wkDay(Date(2010,1,2)) === 6)
		assert(DateMaths.wkDay(Date(2010,1,3)) === 0)
		assert(DateMaths.wkDay(Date(2010,1,4)) === 1)
		assert(DateMaths.wkDay(Date(2010,1,5)) === 2)
		assert(DateMaths.wkDay(Date(2010,1,6)) === 3)
		assert(DateMaths.wkDay(Date(2010,1,7)) === 4)
		assert(DateMaths.wkDay(Date(2010,1,8)) === 5)
		assert(DateMaths.wkDay(Date(2010,1,9)) === 6)
	}
	
	test("cnum"){
		assert(DateMaths.cNum(2000) === 6)
		assert(DateMaths.cNum(1610) === 6)
		assert(DateMaths.cNum(2089) === 6)
		assert(DateMaths.cNum(2111) === 4)
		assert(DateMaths.cNum(2100) === 4)
		assert(DateMaths.cNum(1776) === 4)
		assert(DateMaths.cNum(1987) === 0)
		assert(DateMaths.cNum(1950) === 0)
		assert(DateMaths.cNum(1550) === 0)
		assert(DateMaths.cNum(1450) === 2)
		assert(DateMaths.cNum(1850) === 2)
		assert(DateMaths.cNum(2289) === 2)		
	}

	test("mnum"){
		assert(DateMaths.mNum(fri) == 1)
		assert(DateMaths.mNum(lyear) == 2)
	}
	
	test("isLeap"){
		assert(DateMaths.isLeap(2100) === false )
		assert(DateMaths.isLeap(3000) === false )
		assert(DateMaths.isLeap(2000) === true )
		assert(DateMaths.isLeap(2008) === true )
		assert(DateMaths.isLeap(1999) === false )

	}  

	test("chkDate"){
		assert(DateMaths.chkDate(10000,1,1) === false)
		assert(DateMaths.chkDate(-1,1,1) === false)
		assert(DateMaths.chkDate(2000,-1,1) === false)
		assert(DateMaths.chkDate(2000,13,1) === false)
		assert(DateMaths.chkDate(2000,1, -1) === false)
		assert(DateMaths.chkDate(2001,1,50) === false)
		assert(DateMaths.chkDate(2001,2,29) === false)
	}


	test("day of year"){
		assert(DateMaths.dayOfYear(Date(2000,1,1)) === 1)
		assert(DateMaths.dayOfYear(Date(2000,12,31)) === 366)
		assert(DateMaths.dayOfYear(Date(2001,1,1)) === 1)
		assert(DateMaths.dayOfYear(Date(2001,12,31)) === 365)
	}

	test("days left in year"){
		assert(DateMaths.daysLeftinYear(Date(2001,12, 30)) === 1)
		assert(DateMaths.daysLeftinYear(Date(2000,1, 1)) === 365)
		assert(DateMaths.daysLeftinYear(Date(2001,1, 1)) === 364)
	}


	test("week"){
		assert(DateMaths.week(Date(2013,6,26)) === Week(2013,26))
		assert(DateMaths.week(Date(2013,6,23)) === Week(2013,26))
		assert(DateMaths.week(Date(2013,1,1)) === Week(2013,1))		
		assert(DateMaths.week(Date(2012,12,31)) === Week(2013,1))
		assert(DateMaths.week(Date(2009,12,27)) === Week(2009,53))			
		assert(DateMaths.week(Date(2009,12,28)) === Week(2009,53))
		assert(DateMaths.week(Date(2009,12,29)) === Week(2009,53))			
		assert(DateMaths.week(Date(2009,12,30)) === Week(2009,53))	
		assert(DateMaths.week(Date(2009,12,31)) === Week(2009,53))			
		assert(DateMaths.week(Date(2010,1,1)) === Week(2009,53))
		assert(DateMaths.week(Date(2010,1,2)) === Week(2009,53))
		assert(DateMaths.week(Date(2010,1,3)) === Week(2010,1))		

	}

}

