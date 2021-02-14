package lessons._02.arrays

object Arrays {

  object negative_air_temperatures {
    object for_loop {
      def negative(temperatures: Array[Double]): Int = {
        var days = 0
        for (temp <- temperatures)
          if (temp < 0)
            days += 1
        days
      }
    }

    object count {
      def negative(temperatures: Array[Double]): Int =
        temperatures.count(_ < 0)
    }
  }

  object reversing_an_array {
    object in_place {
      // ⚠🚨 mutates parameter 'a' 😈
      def reverse[A](a: Array[A]): Array[A] = {
        val n = a.length
        for (i <- 0 until n / 2) {
          val k    = n - i - 1
          val temp = a(i)
          a(i) = a(k)
          a(k) = temp
          a(i) = a(i)
        }
        a
      }
    }

    object built_in {
      // builds a copy with the elements reversed
      def reverse[A](a: Array[A]): Array[A] =
        a.reverse
    }
  }

}
