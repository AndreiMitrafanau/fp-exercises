import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable

class SixSpec extends AnyFreeSpec {
  "nonNegativeInt" - {
    "handle min value" in {
      val (number, _) = new Six().nonNegativeInt(new RNG {
        val values: mutable.Stack[Int] = mutable.Stack(Int.MinValue, 42)
        override def nextInt: (Int, RNG) = values.pop() -> this
      })

      assert(number > 0)
    }
  }
}
