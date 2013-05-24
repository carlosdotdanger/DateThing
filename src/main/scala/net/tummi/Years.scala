package net.tummi


class Years(n: Int) extends TimeUnit(n){
	override def toString = "%d months".format(n)
}

object Years{
	def apply (n: Int) = new Years(n)
	def unapply (n: Int): Option[Int] = Some(n)
	def :: (n: Int) = new Years(n)
}
