import benjamingarrett.rockpaperscissorstools.{Paper, RPSMove, RPSUnbiasedRandomMove, Rock, Scissors}
import benjamingarrett.rockpaperscissorstools.RPSUnbiasedRandomMove.SimpleRPSUnbiasedRandomMove

object RandomMoveDemo {
  val moves = Map[RPSMove,String](Rock -> "rock", Paper -> "paper", Scissors -> "scissors")
  def go = {
    val seed = 123
    val random = SimpleRPSUnbiasedRandomMove(seed)
    printMoves(30, random)
    printDoubles(100, random)
  }
  def printMoves(n: Int, rand: RPSUnbiasedRandomMove): Unit = {
    if (n == 0) return
    val (m, r) = rand.unbiasedRandomMove
    println(s"Move ${n}: ${moves(m)}")
    printMoves(n-1, r)
  }
  def printDoubles(n: Int, rand: RPSUnbiasedRandomMove): Unit = {
    if (n == 0) return
    val (m, r) = rand.nextDouble
    println(s"Double $n: $m")
    printDoubles(n - 1, r)
  }
}
