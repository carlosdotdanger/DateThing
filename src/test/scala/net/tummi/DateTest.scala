package net.tummi

import org.scalatest.FunSuite

class DateSuite extends FunSuite{
	
	val w = Date(2000,1,1)
  val x = Date(2001,1,2)
  val y = Date(2001,2,1)
  val z = Date(2002,1,1)
  val l = List(w,x,y,z)


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
  

  test("toString"){
    assert(x.toString === "2001-01-02")
  }

  test("from string"){
    assert(Some(Date(2013,1,1)) === Date("2013-01-01"))
    assert (None === Date("yo mama"))
  }

  test("implicit date to month"){
    assert(Month(2013,1) === takesMonth(Date(2013,1,1)))  
  }

  test("implicit date to year"){
    assert(Year(2013) === takesYear(Date(2013,1,1)))  
  }

  def takesMonth(m: Month) = m
  def takesYear(y: Year) = y
  
  test("every"){
    val days = Date.every()
    assert(days.take(3).toArray === Array(DAY_ZERO, DAY_ZERO + (1 :: Days), DAY_ZERO + (2 :: Days)))
  }

  test("month plus months"){
    val m = Month(2009,11)
    assert(m + (0  :: Months) === Month(2009,11))
    assert(m + (1  :: Months) === Month(2009,12))   
    assert(m + (2  :: Months) === Month(2010,1))
    assert(m + (3  :: Months) === Month(2010,2))
    assert(m + (4  :: Months) === Month(2010,3))   
    assert(m + (5  :: Months) === Month(2010,4))
    assert(m + (6  :: Months) === Month(2010,5))
    assert(m + (7  :: Months) === Month(2010,6))
    assert(m + (8  :: Months) === Month(2010,7))   
    assert(m + (9  :: Months) === Month(2010,8))
    assert(m + (10 :: Months) === Month(2010,9))
    assert(m + (11 :: Months) === Month(2010,10))
    assert(m + (12 :: Months) === Month(2010,11))   
    assert(m + (13 :: Months) === Month(2010,12))
    assert(m + (14 :: Months) === Month(2011,1))
    assert(m + (15 :: Months) === Month(2011,2))
    assert(m + (16 :: Months) === Month(2011,3))
    assert(m + (17 :: Months) === Month(2011,4))
    assert(m + (18 :: Months) === Month(2011,5))   
    assert(m + (19 :: Months) === Month(2011,6))
    assert(m + (20 :: Months) === Month(2011,7))
    assert(m + (21 :: Months) === Month(2011,8))
    assert(m + (22 :: Months) === Month(2011,9))
    assert(m + (23 :: Months) === Month(2011,10))
    assert(m + (24 :: Months) === Month(2011,11))
    assert(m + (25 :: Months) === Month(2011,12))
    assert(m + (26 :: Months) === Month(2012,1))   
    assert(m + (27 :: Months) === Month(2012,2))
    assert(m + (28 :: Months) === Month(2012,3))
    assert(m + (29 :: Months) === Month(2012,4))
    assert(m + (-1  :: Months) === Month(2009,10))   
    assert(m + (-2  :: Months) === Month(2009,9))
    assert(m + (-3  :: Months) === Month(2009,8))
    assert(m + (-4  :: Months) === Month(2009,7))   
    assert(m + (-5  :: Months) === Month(2009,6))
    assert(m + (-6  :: Months) === Month(2009,5))
    assert(m + (-7  :: Months) === Month(2009,4))
    assert(m + (-8  :: Months) === Month(2009,3))   
    assert(m + (-9  :: Months) === Month(2009,2))
    assert(m + (-10 :: Months) === Month(2009,1))
    assert(m + (-11 :: Months) === Month(2008,12))
    assert(m + (-12 :: Months) === Month(2008,11))
  }
  test("month minus months"){
    val m = Month(2009,11)
    assert(m - (0  :: Months) === Month(2009,11))
    assert(m - (1  :: Months) === Month(2009,10))   
    assert(m - (2  :: Months) === Month(2009,9))
    assert(m - (3  :: Months) === Month(2009,8))
    assert(m - (4  :: Months) === Month(2009,7))   
    assert(m - (5  :: Months) === Month(2009,6))
    assert(m - (6  :: Months) === Month(2009,5))
    assert(m - (7  :: Months) === Month(2009,4))
    assert(m - (8  :: Months) === Month(2009,3))   
    assert(m - (9  :: Months) === Month(2009,2))
    assert(m - (10 :: Months) === Month(2009,1))
    assert(m - (11 :: Months) === Month(2008,12))
    assert(m - (12 :: Months) === Month(2008,11))   
    assert(m - (13 :: Months) === Month(2008,10))
    assert(m - (14 :: Months) === Month(2008,9))
    assert(m - (15 :: Months) === Month(2008,8))
    assert(m - (16 :: Months) === Month(2008,7))
    assert(m - (17 :: Months) === Month(2008,6))
    assert(m - (18 :: Months) === Month(2008,5))   
    assert(m - (19 :: Months) === Month(2008,4))
    assert(m - (20 :: Months) === Month(2008,3))
    assert(m - (21 :: Months) === Month(2008,2))
    assert(m - (22 :: Months) === Month(2008,1))
    assert(m - (23 :: Months) === Month(2007,12))
    assert(m - (24 :: Months) === Month(2007,11))
    assert(m - (25 :: Months) === Month(2007,10))
    assert(m - (26 :: Months) === Month(2007,9))   
    assert(m - (27 :: Months) === Month(2007,8))
    assert(m - (28 :: Months) === Month(2007,7))
    assert(m - (29 :: Months) === Month(2007,6))
    assert(m - (-1  :: Months) === Month(2009,12))   
    assert(m - (-2  :: Months) === Month(2010,1))
    assert(m - (-3  :: Months) === Month(2010,2))
    assert(m - (-4  :: Months) === Month(2010,3))   
    assert(m - (-5  :: Months) === Month(2010,4))
    assert(m - (-6  :: Months) === Month(2010,5))
    assert(m - (-7  :: Months) === Month(2010,6))
    assert(m - (-8  :: Months) === Month(2010,7))   
    assert(m - (-9  :: Months) === Month(2010,8))
    assert(m - (-10 :: Months) === Month(2010,9))
    assert(m - (-11 :: Months) === Month(2010,10))
    assert(m - (-12 :: Months) === Month(2010,11))
  }

}
