
object Six {
  /**
   * Generates a random integer between 0 and Int.MaxValue (inclusive).
   * Has Int.MinValue handling
   *
   * @param rng context for generating nextInt
   * @return tuple of random Int and next [[RNG]]
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  /**
   * Generates a Double between 0 and 1, not including 1
   *
   * @param rng context for generating nextInt
   * @return tuple of double between 0 and 1 exclusive and next [[RNG]]
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (s"0.$i".toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  /**
   * Generates a list of random integers.
   *
   * @param count Int starting from 0
   * @param rng context for generating nextInt
   * @return tuple with List of values and new [[RNG]] context
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = count match {
      case _ if count <= 0 => acc -> rng
      case _ =>
        val (i, rng2) = rng.nextInt
        go(count - 1, rng2, i :: acc)
    }

    go(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleUsingMap(rng: RNG): (Double, RNG) = {
    map(_ => nonNegativeInt(rng))((i: Int) => (s"0.$i".toDouble)).apply(rng)
  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (i1, rng1) = ra(rng)
      val (i2, rng2) = rb(rng1)
      (f(i1, i2), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

}


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

