class Six {
  /**
   * Generates a random integer between 0 and Int.MaxValue (inclusive).
   * Has Int.MinValue handling
   *
   * @param rng context for generating nextInt
   * @return tuple of random Int and next [[RNG]]
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if(i == Int.MinValue || i < 0) nonNegativeInt(rng2)
    else (i, rng2)
  }

}


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed:Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

