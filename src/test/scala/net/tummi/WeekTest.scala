package net.tummi

import org.scalatest.FunSuite
import net.tummi.DateMaths._

class WeekSuite extends FunSuite{

	
	test("check week start and end"){
		val d = Date(2013,6,23) //sunday
		val w: Week = d
		assert(w.start === Date(2013,6,17))
		assert(w.end === d)
		assert(d.day === 6)
	}




	test("check mondays"){
		val i = Date.every(from = Date(1970,1,1)).take(2000000).iterator
		for(d <- i ){ 
			val w: Week = d
			assert(w.start.day === 0)
		}
	}

	test("check sundays"){
		val i = Date.every(from = Date(1970,1,1)).take(2000000).iterator
		for(d <- i ){ 
			val w: Week = d
			assert(w.end.day === 6)
		}
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