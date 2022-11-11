import scala.annotation.tailrec
import scala.io.StdIn.readLine
object TraditionalMain {

  def sumSquaresIterative(n: Int): Int = {
    var y = 0
    for (x <- 1 to n) {
      y += x * x
    }
    y // the value of the function is the last value in the block
  }

  def sumSquaresRecursive(n: Int): Int =
    if (n == 0) 0
    else n * n + sumSquaresRecursive(n - 1)

  val sumSquaresRecursiveVal: Int => Int = n => if (n == 0) 0 else n * n + sumSquaresRecursiveVal(n - 1)

  def sumSquaresFunctional(n: Int): Int = (1 to n).map(x => x * x).sum

  def sumSquaresPatternMatching(n: Int): Int =
    n match {
      case 0 => 0
      case x => x * x + sumSquaresPatternMatching(n - 1)
    }

  def sumMappedRange(f: Int => Int)(n: Int): Int = (1 to n).map(f).sum

  def isSorted[A](a: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= a.length - 1) true
      else if (gt(a(n), a(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  // pattern matching
  def fib1(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib1(n - 1) + fib1(n - 2)
    }
  }

  def fib2(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  type one_at_a_time[A, B, C] = A => B => C
  type two_at_a_time[A, B, C] = (A, B) => C

  def curryV2[A, B, C](f: two_at_a_time[A, B, C]): one_at_a_time[A, B, C] = a => b => f(a, b)

  def uncurryV2[A, B, C](f: one_at_a_time[A, B, C]): two_at_a_time[A, B, C] = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def ourpartial[A, B, C, D](a: A, f: (A, B, C) => D): B => C => D = (b: B) => (c: C) => f(a, b, c)

  def simpleInterest(p: Double, r: Double, t: Double): Double = p * r * t / 100

  def interestOnTenK(r: Double, t: Double): Double = ourpartial(10000.0, simpleInterest)(r)(t)

  def twentyFivePercentInterestOnTenK(t: Double): Double = ourpartial(10000.0, simpleInterest)(25)(t)

  def go = {
    println("Here are some examples of Scala code")
    val x = 10 // defines an immutable value having inferred type Int
    var a: Double = 10.0 // defines a mutable value having inferred type Double
    val scalaIsCool = true // inferred type Boolean, note Boolean literals true and false
    // are lowercase just like in Java. Scala compiles to JVM runnable
    // byte code.
    val y = "hello" // String literals can only be surrounded by double quotes " characters
    // and not apostrophes ' like in Python
    val z: Any = "some string" // forcing the type to be a supertype of the value assigned
    val list: List[Any] = List(1, "3", true) // lists can have heterogenous types
    val map: Map[Any, Any] = Map(1 -> "one", "two" -> 2, "three" -> 2) // maps can also have heterogeneous types
    val tuple1: (Int, String) = (1, "one")
    val tuple2: (String, Int) = ("two", 2)
    val listOfTuples: Seq[(Any, Any)] = List(tuple1, tuple2) // I'm putting in the type annotations
    // everywhere here, but they're optional
    // whenever the type in question
    // can be inferred by the compiler
    val semicolonsAreOptionalToo = true; // sometimes people put several items on one line
    // so semicolons come in handy
    // like in this case
    // a block is what's surrounded by curly braces. The following block has two statements.
    // the value of the block is the value of the last expression in it.
    val myValue: String = {
      println(s"I'm setting myValue to ${y}");
      y
    }
    // this conditional statement has a type for the same reason
    if (x > 5) {
      println("x is greater 5")
      3.14
    } else {
      println("x wasn't big enough")
      "foo"
    }
    // since it isn't known at compile time which branch will be taken
    // the compiler ascribes type Any to the following conditional
    val cond: Any = if (x > 5) {
      println("x is greater 5")
      3.14
    } else {
      println("x wasn't big enough")
      "foo"
    }
    // in this case both blocks have type Double, so the compiler is able to
    // infer type Double for the whole expression
    val cond2: Double = if (x > 5) {
      println("x is greater 5")
      3.14
    } else {
      println("x wasn't big enough")
      2.72
    }
    // in cond3 below the type of argument xx is needed, plus parens to make the expression work
    // when type of overall expression isn't declared
    val cond3 =
    (xx: Int) => if (xx > 5) {
      println("big enough");
      "big"
    } else {
      println("too small");
      "small"
    }
    // in cond4 the overall type of the expression, namely a function taking an Int and returning
    // a String, allows us to omit the type specification of parameter xx
    val cond4: Int => String =
    xx => if (xx > 5) {
      println("big enough");
      "big"
    } else {
      println("too small");
      "small"
    }
    //
    println(s"conditional lambda (first way): ${cond3(10)}")
    println(s"conditional lambda (second way): ${cond4(10)}")
    // by default maps are immutable
    val myMap = Map("golden ratio" -> 1.618, "Euler's number" -> 2.718)
    // Retrieve the value associated with a given key using parentheses
    println(s"The golden ratio is approximately ${myMap("golden ratio")}")

    import scala.collection.mutable.Map
    // in order to change what's in a map, you need to import the mutable version first
    val m = Map(1 -> 2)
    m += {
      3 -> 4 // adds a key/value pair
    }
    // practice with some of the functions defined below
    val sumSquareIter = sumSquaresIterative(10)
    val sumSquareRecu = sumSquaresRecursive(10)
    val sumSquareRecVal = sumSquaresRecursiveVal(10)
    val sumSquareFunc = sumSquaresFunctional(10)
    val sumSquarePatt = sumSquaresPatternMatching(10)
    println(s"sum of squares five ways: ${sumSquareIter}, ${sumSquareRecu}, ${sumSquareRecVal}, ${sumSquareFunc}, ${sumSquarePatt}")
    val sumCubes: Int => Int = sumMappedRange(x => x * x * x)
    val sumRoots: Int => Int = sumMappedRange(x => Math.sqrt(x.toDouble).toInt)
    val sumRationalExpr: Int => Int = sumMappedRange(x => (x * x + 1) / x)
    val sumCubes10 = sumCubes(10)
    val sumRoots100 = sumRoots(100)
    val sumRationals100 = sumRationalExpr(100)
    println(s"Using sumMappingRange: ${sumCubes10}, ${sumRoots100}, ${sumRationals100}")
    println(s"Using isSorted: ${isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => true)}")
    println(s"Using fib1: ${fib1(1)}")
    println(s"Using fib1: ${fib1(2)}")
    println(s"Using fib1: ${fib1(3)}")
    println(s"Using fib1: ${fib1(4)}")
    println(s"Using fib1: ${fib1(5)}")
    println(s"Using fib1: ${fib2(100000)}") // note this does not blow up the stack
    println(s"Simple interest on $$10,000 by providing everything at once: ${simpleInterest(10000,25,5)}")
    println(s"Simple interest on $$10,000 using partial application: ${interestOnTenK(25,5)}")
    println(s"Simple interest on $$10,000 providing time duration at the last moment: ${twentyFivePercentInterestOnTenK(5)}")
    /*val weirdPatternMatch = (A: Foo) match {
      case "should not match" => 3.4
      case "did not match" => 5.6
      case "would never match" => 7.8
    }
    println(s"Weird pattern match gives: ${weirdPatternMatch}")*/
    val flat1 = (x: Int) => if(x == 0) None else Some(1/x)
    val flat2 = (x: Int) => if(x >= 0) Math.sqrt(x) else None
    val flat3: List[Int] = List(1,2,3,4,5,6,7,8,9).flatMap(flat1)
    println("Enter an integer: ")
    val number: Int = readLine().toInt
    println(s"You entered: ${number}")
    println("Enter another integer: ")
    val number2 = readLine().toInt
    println(s"You entered: ${number2}")
    MidtermPracticeExamSolutions


  }

  def main(args: Array[String]): Unit = {
    println("Hello from main of object")
    go
    ParsingDemo.go
  }
}
