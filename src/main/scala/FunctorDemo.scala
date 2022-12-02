object FunctorDemo {

  def f(x: Int): Double = (x + 2) / 7.0

  def g(y: Double): Boolean = y > 0.85

  def go = {
    val res1 = List(1,2,3,4,5,6,7,8).map(f).map(g)
    val res2 = List(1,2,3,4,5,6,7,8).map(x => g(f(x)))
    println(s"$res1  $res2")
  }
}
