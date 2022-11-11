object FlatMapDemo {

  def f(x: Int): Option[Int] = Some(x)

  def g(y: Int): Option[Double] = Some(y.toDouble)

  def h(z: Double): Option[String] = Some(z.toString)

  def k(a: String): Boolean = a.length > 0

  def go = {
    val resultUsingForComprehension = sampleMonadicCompositionUsingForComprehension(10)
    val resultUsingFlatMap = sampleMonadicCompositionUsingFlatMap(10)
    println(s"Monadic composition (for comprehension): ${resultUsingForComprehension}")
    println(s"Monadic composition (flatMap): ${resultUsingFlatMap}")
  }

  def sampleMonadicCompositionUsingForComprehension(x: Int): Option[Boolean] =
    for {
      y <- f(x)
      z <- g(y)
      a <- h(z)
    } yield k(a)

  def sampleMonadicCompositionUsingFlatMap(x: Int): Option[Boolean] =
    f(x).flatMap((y: Int) => g(y).flatMap((z: Double) => h(z))).map((a: String) => k(a))
}
