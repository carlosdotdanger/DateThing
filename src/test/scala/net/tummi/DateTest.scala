package net.tummi

import org.scalatest.FunSuite

class DateSuite extends FunSuite{
	
	val w = Date(2001,1,1)
  val x = Date(2001,1,2)
  val y = Date(2001,2,1)
  val z = Date(2002,1,1)
  val l = List(w,x,y,z)

  test("UFO"){
   val res = for(x1 <-l; x2 <- l) yield x1 <=> x2
   val expected = List(0,-1,-1,-1,1,0,-1,-1,1,1,0,-1,1,1,1,0)
   assert(res == expected)
  }

  test("lt"){
    val res = for(x1 <-l; x2 <- l) yield x1 < x2
    val expected = List(false,true,true,true,false,false,true,true,false,false,false,true,false,false,false,false)
    assert(res == expected)
  }

  test("gt"){
    val res = for(x1 <-l; x2 <- l) yield x1 > x2
    val expected = List(false,false,false,false,true,false,false,false,true,true,false,false,true,true,true,false)
    assert(res == expected)
  }

  test("lte"){
    val res = for(x1 <-l; x2 <- l) yield x1 <= x2
    val expected = List(true,true,true,true,
												false,true,true,true,
												false,false,true,true,
												false,false,false,true)
    assert(res == expected)
  }

  test("gte"){
    val res = for(x1 <-l; x2 <- l) yield x1 >= x2
    val expected = List(true,false,false,false,true,true,false,false,true,true,true,false,true,true,true,true)
    assert(res == expected)
  }

  test("eq"){
    val res = for(x1 <-l; x2 <- l) yield x1 == x2
    val expected = List(true,false,false,false,false,true,false,false,false,false,true,false,false,false,false,true)
    assert(res == expected)
  }

}
