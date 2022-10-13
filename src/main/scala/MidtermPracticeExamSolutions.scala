object MidtermPracticeExamSolutions {

  // 3
  val m: Map[String,Int] = Map("foo" -> 2, "bar" -> 7)
  val n: Option[Int] = m.get("foo")
  val p: Option[Int] = m.get("baz")
  // a. What is the type of n? Option[Int]
  // b. What is the type of p? Option[Int]
  n match {
    case Some(value) => println(s"foo is in the dictionary with value ${value}")
    case None => println("foo not found")
  }
  p match {
    case Some(value) => println(s"baz is in the dictionary with value ${value}")
    case None => println("baz not found")
  }

  // 4
  // a. method signature of main is def main(args: Array[String]): Unit
  // b. type of main method is Array[String] => Unit

  // 5
  // a. Use filter to remove odd numbers from a list of integers from 1 to 10
  val fiveA = (1 to 10).toList.filter(k => k % 2 == 0)
  println(s"5a. remove odd numbers from list using filter: ${fiveA}")
  // b. Use map to add 7 to every element in a list of integers from 1 to 10
  val fiveB = (1 to 10).toList.map(k => k + 7)
  println(s"5b. add 7 to every element: ${fiveB}")
  // c. Use reduce to sum up all numbers in a list
  val fiveC = (1 to 10).toList.reduce(_ + _)
  println(s"5c. sum up numbers in list: ${fiveC}")
  // d. Combine the above three techniques using functional composition into a single expression
  //    that removes odd numbers, adds 7 to each remaining element, and sums up the result.
  val fiveD: Int = (1 to 10).toList.filter(k => k % 2 == 0).map(k => k + 7).reduce(_ + _)
  println(s"5d. combine all three techniques: ${fiveD}")
  // e. what is the type of the previous result? Int

  // 6
  // a. Polymorphic function curry
  def curry[A,B,C](f: (A,B) => C): A => B => C = (a: A) => (b: B) => f(a,b)
  // b. type is (A,B) => C => A => B => C

  // 7
  // a. Uncurried triangleArea
  def triangleArea(base: Double, height: Double): Double = 0.5 * base * height
  // b. type is (Double, Double) => Double
  val ta: (Double, Double) => Double = triangleArea
  // c. create curried version
  val triangleAreaCurried: Double => Double => Double = curry(triangleArea)
  // d. use this curried version
  val exampleArea = triangleAreaCurried(7)(11)
  println(s"Area of triangle having base = 7 and height = 11 is ${exampleArea}")
  // e. type of triangleAreaCurried is Double => Double => Double

  // 8
  // a. Implement threeCopies
  def threeCopies[A](c: List[A]): List[A] = c.flatMap(k => List(k,k,k))
  val eightA: List[Any] = threeCopies(List("foo", 3.14, 42, true))
  println(s"using threeCopies: ${eightA}")
  // b. type of threeCopies is List[A] => List[A]

  // 9
  // a. Implement copiedOdds
  def copiedOdds(c: List[Int]): List[Int] = c.filter(k => k % 2 != 0).flatMap(x => List(x,x))
  val nineA: List[Int] = copiedOdds((1 to 10).toList)
  println(s"using copiedOdds ${nineA}")

  // 10
  val ten = List(1,2,3,4).map(v => v.toString).reduce(_ + " and " + _)
  println(s"Exercise ten: ${ten}")

  // 11
  // a. remove even elements from it and reverse it
  def f[A](x: List[A], g: A => List[A]): List[A] = x.flatMap(g)
  val elevenA = f(List(1,2,3,4,5,6), (k: Int) => if (k % 2 != 0) List(k) else List()).reverse
  println(s"eleventA ${elevenA}")
  // b. type of f is (List[A], A => List[A]) => List[A]

}
