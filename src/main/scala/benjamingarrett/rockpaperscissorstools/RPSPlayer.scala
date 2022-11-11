package benjamingarrett.rockpaperscissorstools

import benjamingarrett.rockpaperscissorstools.RNG.Rand

sealed trait RPSPlayer {
  val playerInfo: String
}

trait RPSHistoryBasedPlayer extends RPSPlayer {
  def playMove(history: List[(RPSMove,RPSOutcome)]): RPSMove
}

trait RPSHistoryBasedRandomPlayer extends RPSPlayer {
  def playMove(history: List[(RPSMove, RPSOutcome)], random: Rand[Int]): (RPSMove, Rand[Int])
}