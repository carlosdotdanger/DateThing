package net.tummi

import org.scalatest.FunSuite

class DSuite extends FunSuite{

	test("toDays : day zero" ){
		assert(DateMaths.toDays(DAY_ZERO) === 0)
	}

	test("toDate : zero"){
		assert(DateMaths.toDate(0) === DAY_ZERO)
	}


	test("toDays"){
		var counter = 0L
		val i = DateMaths.every().take(1).iterator
		for(d <- i ){ 
			assert(DateMaths.toDays(d) === counter)
			assert(DateMaths.toDate(counter) === d)
			counter += 1
		}
	}
 	test("to Date Static"){
		assert(DateMaths.toDate(0) === DAY_ZERO)
		assert(DateMaths.toDate(1) === Date(0,3,2) )
		assert(DateMaths.toDate(365) === Date(1,3,1)) 
		assert(DateMaths.toDate(730) === Date(2,3,1))
		assert(DateMaths.toDate(1461) === Date(4,3,1))
		assert(DateMaths.toDate(14610) === Date(40,3,1))
		assert(DateMaths.toDate(146097) === Date(400,3,1))
		assert(DateMaths.toDate(292194) === Date(800,3,1))
		assert(DateMaths.toDate(584388) === Date(1600,3,1))
	}
	test("to days and back"){
		val s = Date(2001,1,1)
		assert(DateMaths.toDate(DateMaths.toDays(s)) === s)
	}
}
