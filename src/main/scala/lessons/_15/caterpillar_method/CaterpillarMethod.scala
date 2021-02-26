package lessons._15.caterpillar_method

import scala.annotation.tailrec

object CaterpillarMethod {

  object caterpillar_method {

    object procedural_style {
      def caterpillarMethod(a: Array[Int], sum: Int): Boolean = {
        val n            = a.length
        var front, total = 0
        for (back <- a.indices) {
          while (front < n && total + a(front) <= sum) {
            total += a(front)
            front += 1
          }
          if (total == sum)
            return true
          total -= a(back)
        }
        false
      }
    }

    object tail_recursive {
      def caterpillarMethod(a: Array[Int], sum: Int): Boolean = {
        val n = a.length

        @tailrec def loop(front: Int, total: Int, back: Int): Boolean =
          if (back < n)
            if (front < n && total + a(front) <= sum)
              loop(front + 1, total + a(front), back)
            else if (total == sum) true
            else loop(front, total - a(back), back + 1)
          else false
        loop(front = 0, total = 0, back = 0)
      }
    }

    object functional_style {

      def caterpillarMethod(a: Array[Int], sum: Int): Boolean = {
        val n = a.length
        a.iterator
          .scanLeft((0, 0)) { case ((front, total), back) =>
            def forward  = a.iterator.slice(front, n).scanLeft(total)(_ + _).takeWhile(_ <= sum)
            val newTotal = forward.reduce((_, total) => total)
            val newFront = front + forward.size - 1
            (newFront, if (newTotal == sum) newTotal else newTotal - back)
          }
          .exists { case (_, total) => total == sum }
      }
    }
  }

  object count_triangles {

    object procedural_style {
      def countTriangles(a: Array[Int]) = {
        val n      = a.length
        var result = 0
        for (x <- a.indices) {
          var z = x + 2
          for (y <- x + 1 until n) {
            while (z < n && a(x) + a(y) > a(z))
              z += 1
            result += z - y - 1
          }
        }
        result
      }
    }

    object functional_style {
      def countTriangles(a: Array[Int]) =
        a.indices.map { x =>
          val yRange     = a.indices.drop(x + 1)
          val zStart     = yRange.foldLeft((x + 2, 0)) _
          val (_, count) = zStart { case ((z, count), y) =>
            val zRange = a.indices.drop(z)
            val zLast  = z + zRange.count(z => a(x) + a(y) > a(z))
            (zLast, count + zLast - y - 1)
          }
          count
        }.sum
    }
  }
}
