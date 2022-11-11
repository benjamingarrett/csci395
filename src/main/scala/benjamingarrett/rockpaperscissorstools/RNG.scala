package benjamingarrett.rockpaperscissorstools

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
}



