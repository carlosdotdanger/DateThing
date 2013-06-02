package net.tummi

import org.scalatest.FunSuite

class DateSuite extends FunSuite{
	
	val w = Date(2000,1,1)
  val x = Date(2001,1,2)
  val y = Date(2001,2,1)
  val z = Date(2002,1,1)
  val l = List(w,x,y,z)

  test("UFO"){
   val res = for(x1 <-l; x2 <- l) yield x1 <=> x2
   val expected = List(0,-1,-1,-2,1,0,-1,-1,1,1,0,-1,2,1,1,0)
   assert(res === expected)
  }

  test("lt"){
    val res = for(x1 <-l; x2 <- l) yield x1 < x2
    val expected = List(false, true,  true,  true,
                        false, false, true,  true,
                        false, false, false, true,
                        false, false, false, false)
    assert(res === expected)
  }

  test("gt"){
    val res = for(x1 <-l; x2 <- l) yield x1 > x2
    val expected = List(false, false, false, false,
                        true,  false, false, false,
                        true,  true,  false, false,
                        true,  true,  true,  false)
    assert(res === expected)
  }

  test("lte"){
    val res = for(x1 <-l; x2 <- l) yield x1 <= x2
    val expected = List(true,  true,  true,  true,
												false, true,  true,  true,
												false, false, true,  true,
												false, false, false, true)
    assert(res === expected)
  }

  test("gte"){
    val res = for(x1 <-l; x2 <- l) yield x1 >= x2
    val expected = List(true, false, false, false,
                        true, true,  false, false,
                        true, true,  true,  false,
                        true, true,  true,  true)
    assert(res === expected)
  }

  test("eq"){
    val res = for(x1 <-l; x2 <- l) yield x1 == x2
    val expected = List(true,  false, false, false,
                        false, true,  false, false,
                        false, false, true,  false,
                        false, false, false, true)
    assert(res === expected)
  }



  test("plus days"){
    assert(w + (366 :: Days) === Date(2001,1,1))
    assert(x + (365 :: Days) === Date(2002,1,2))
    assert(x + (0 :: Days) === x)
  }

  test("minus days"){
    assert(w - (366 :: Days) === Date(1998,12,31))
    assert(x - (365 :: Days) === Date(2000,1,3))
    assert(x - (0 :: Days) === x)
  }
  
  test("plus months"){
    assert(w + (10 :: Months) === Date(2000,11,1))
    assert(x + (24 :: Months) === Date(2003,1,2))
    assert(x + (120000 :: Months) === Date(12001,1,2))
    assert(Date(2010,12,31) + (13 :: Months) === Date(2012,1,31))
  }

  test("minus months"){
    assert(Date(2001,12,1) - (13 :: Months) === Date(2000,11,1))
    assert(Date(2001,12,1) - (2 :: Months) === Date(2001,10,1))
    assert(Date(2001,12,1) - (26 :: Months) === Date(1999,10,1))
  }

  test("plus years"){
    assert(w + (366 :: Years) === Date(2366,1,1))
    assert(x + (3 :: Years) === Date(2004,1,2))
  }

  test("minus years"){
    assert(w - (366 :: Years) === Date(1634,1,1))
    assert(x - (3 :: Years) === Date(1998,1,2))
  }

  test("toString"){
    assert(x.toString === "2001-01-02")
  }

}
