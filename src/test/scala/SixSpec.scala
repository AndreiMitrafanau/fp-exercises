import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable
import Six._

class SixSpec extends AnyFreeSpec {
  lazy val simpleRNG = SimpleRNG(Int.MinValue)

  "nonNegativeInt" - {
    "handle min value" in {
      val (number, _) = nonNegativeInt(new RNG {
        val values: mutable.Stack[Int] = mutable.Stack(Int.MinValue, 42)

        override def nextInt: (Int, RNG) = values.pop() -> this
      })

      assert(number > 0)
    }
  }
  "double" - {
    "return value between 0 and 1 exclusive" in {
      val (number, _) = double(simpleRNG)
      assert(number > 0)
      assert(number < 1)
    }
  }

  "ints" - {
    "fill list with random ints" in {
      val (list, _) = ints(10)(simpleRNG)

      assert(list.size == 10)
    }

    "return empty list when count is 0" in {
      val (list, _) = ints(0)(simpleRNG)

      assert(list.isEmpty)
    }

    "return empty list when count is less than 0" in {
      val (list, _) = ints(-1)(simpleRNG)

      assert(list.isEmpty)
    }
  }
}
