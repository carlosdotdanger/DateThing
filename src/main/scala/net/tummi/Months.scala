package net.tummi

class Months(n: Int) extends TimeUnit(n){
	override def toString = "%d months".format(n)
}
object Months{
	def apply (n: Int) = new Months(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Months(n)
}


