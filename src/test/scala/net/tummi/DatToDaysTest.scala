package net.tummi

import org.scalatest.FunSuite
import net.tummi.DateMaths._

class DateAndDaysSuite extends FunSuite{

	test("toDays : day zero" ){
		assert(toDays(DAY_ZERO) === 0)
	}

	test("toDate : zero"){
		assert(toDate(0) === DAY_ZERO)
	}


	test("toDays"){
		var counter = 0L
		val i = Date.every().take(3000000).iterator
		for(d <- i ){ 
			assert(toDays(d) === counter)
			assert(toDate(counter) === d)
			counter += 1
		}
	}
 	test("to Date"){
 		for(i <- 0 to 3000000 )
			assert(toDate(i) === DAY_ZERO + (i :: Days))
	}

	test("to days and back"){
		val i = Date.every().take(3000000).iterator	
		var ed: Date = DAY_ZERO 
		for(d <- i )
			assert(toDate(toDays(d)) === d)
	}
}
