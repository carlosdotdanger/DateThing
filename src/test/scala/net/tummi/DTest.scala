package net.tummi

import org.scalatest.FunSuite

class DSuite extends FunSuite{

	test("toDays : day zero" ){
		assert(D.toDays(DAY_ZERO) === 0)
	}

	test("toDate : zero"){
		assert(D.toDate(0) === DAY_ZERO)
	}


	test("toDays"){
		var counter = 0L
		val i = DateMaths.every().take(1).iterator
		for(d <- i ){ 
			assert(D.toDays(d) === counter)
			assert(D.toDate(counter) === d)
			counter += 1
		}
	}
 	test("to Date Static"){
		assert(D.toDate(0) === DAY_ZERO)
		assert(D.toDate(1) === Date(0,3,2) )
		assert(D.toDate(365) === Date(1,3,1)) 
		assert(D.toDate(730) === Date(2,3,1))
		assert(D.toDate(1461) === Date(4,3,1))
		assert(D.toDate(14610) === Date(40,3,1))
		assert(D.toDate(146097) === Date(400,3,1))
		//assert(D.toDate(292194) === Date(800,3,1))
		assert(D.toDate(584388) === Date(1600,3,1))
	}
	test("to days and back"){
		val s = Date(2001,1,1)
		assert(D.toDate(D.toDays(s)) === s)
	}
}
