package net.tummi


class Days(n: Int) extends TimeUnit(n){
	override def toString = "%d days".format(n)
}
object Days{
	def apply (n: Int) = new Days(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Days(n)
}

