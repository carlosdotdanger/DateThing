package net.tummi

import org.scalatest.FunSuite
import net.tummi.DateMaths._

class WeekSuite extends FunSuite{


	test("check mondays and fridays"){
		var counter = 0L
		val i = Date.every().take(3000000).iterator
		for(d <- i ){ 
			assert(toDays(d) === counter)
			assert(toDate(counter) === d)
			counter += 1
		}
	}
}