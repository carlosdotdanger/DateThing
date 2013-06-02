package net.tummi 

import org.scalatest.FunSuite

class DateMathsSuite extends FunSuite{
	val fri = Date(2013,5,24)
	val lyear = Date(2000,2,29)

	test("wkDay"){
		assert(DateMaths.wkDay(fri) === 5)
		assert(DateMaths.wkDay(lyear) === 2)
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

	test("leapsBetween"){
		assert(DateMaths.leapsBetween(lyear,fri) === 3)
	}
	
	test("isLeap"){
		assert(DateMaths.isLeap(2100) === false )
		assert(DateMaths.isLeap(3000) === false )
		assert(DateMaths.isLeap(2000) === true )
		assert(DateMaths.isLeap(2008) === true )
		assert(DateMaths.isLeap(1999) === false )

	}  
	test("every"){
    	val days = DateMaths.every()
    	assert(days.take(3).toArray === Array(DAY_ZERO, DAY_ZERO + (1 :: Days), DAY_ZERO + (2 :: Days)))
  	}


}
