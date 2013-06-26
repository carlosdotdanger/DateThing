package net.tummi


abstract class TimeUnit(val n: Int)


class Years(n: Int) extends TimeUnit(n){
	override def toString = "%d years".format(n)
}

object Years{
	def apply (n: Int) = new Years(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Years(n)
}

class Months(n: Int) extends TimeUnit(n){
	override def toString = "%d months".format(n)
}

object Months{
	def apply (n: Int) = new Months(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Months(n)
}


class Weeks(n: Int) extends TimeUnit(n){
	override def toString = "%d weeks".format(n)
}

object Weeks{
	def apply (n: Int) = new Weeks(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Weeks(n)
}

class Days(n: Int) extends TimeUnit(n){
	override def toString = "%d days".format(n)
}
object Days{
	def apply (n: Int) = new Days(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Days(n)
}

 