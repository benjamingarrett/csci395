object TraditionalMain {

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
    val list: Seq[Any] = List(1, "3", true) // lists can have heterogenous types
    val map = Map(1 -> "one", "two" -> 2) // maps can also have heterogeneous types
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
      println(s"I'm setting myValue to ${y}"); y
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
      println("big enough"); "big"
    } else {
      println("too small"); "small"
    }
    // in cond4 the overall type of the expression, namely a function taking an Int and returning
    // a String, allows us to omit the type specification of parameter xx
    val cond4: Int => String =
    xx => if (xx > 5) {
      println("big enough"); "big"
    } else {
      println("too small"); "small"
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
      3 -> 4  // adds a key/value pair
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

  }

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
  def main(args: Array[String]): Unit = {
    println("Hello from main of object")
    go
  }
}
