package net.tummi

object D{

//function g(y,m,d)
//m = (m + 9) % 12
//y = y - m/10
//return 365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + ( d - 1 )


	def toDays(d:Date):Long = {
		val m = (d.m + 9) % 12
		val y = d.y - m/10
		lf(y) + ((m * 306) + 5)/10 + (d.d -1)
	}


//function d(g)
//y = (10000*g + 14780)/3652425
//ddd = g - (365*y + y/4 - y/100 + y/400)
//if (ddd < 0) then
// y = y - 1
// ddd = g - (365*y + y/4 - y/100 + y/400)
// endif
//mi = (100*ddd + 52)/3060
//mm = (mi + 2)%12 + 1
//y = y + (mi + 2)/12
//dd = ddd - (mi*306 + 5)/10 + 1
//return y, mm, dd

	
	def toDate(x: Long): Date ={
		var y = (10000L * x + 14780L)/3652425L	
		var ddd = x - lf(y)
		if(ddd < 0){
			y = y - 1
			ddd = x - lf(y) 
		}
		val mi = (100L * ddd + 52)/3060L
		val mm = (mi + 2) % 12 + 1
		y = y + (mi + 2)/12
		val dd = ddd - ((mi*306) + 5)/10 + 1
		Date(y.asInstanceOf[Int], mm.asInstanceOf[Int], dd.asInstanceOf[Int])
	}
	
	def lf(y: Long) = (365*y) + y/4 - y/100 + y/400
}
