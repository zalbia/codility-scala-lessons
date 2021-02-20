package lessons._10.prime_and_composite_numbers

object PrimeAndCompositeNumbers {

  object counting_divisors { // O(sqrt(n))

    object procedural_style {
      def divisors(n: Int): Int = {
        var i      = 1
        var result = 0
        while (i * i < n) {
          if (n % i == 0)
            result += 2
          i += 1
        }
        if (i * i == n)
          result += 1
        result
      }
    }

    object functional_style {
      import Iterator.from

      def divisors(n: Int): Int = {
        val (count, finalSquare) = from(1)
          .takeWhile(i => i * i <= n)
          .foldLeft((0, 1)) { case ((count, _), i) =>
            (if (n % i == 0 && i * i != n) count + 2 else count, i * i)
          }
        if (finalSquare == n) count + 1 else count
      }
    }
  }

  object primality_test { // O(sqrt(n))

    object procedural_style {
      def isPrime(n: Int): Boolean = {
        var i = 2
        while (i * i <= n) {
          if (n % i == 0) {
            return false
          }
          i += 1
        }
        true
      }
    }

    object functional_style {
      import Iterator.from

      def isPrime(n: Int): Boolean =
        from(2).takeWhile(i => i * i <= n).forall(n % _ != 0)
    }
  }

  object reversing_coins {

    object n_log_n { // O(n log n)

      object procedural_style {
        // much faster because array
        def countTails(n: Int): Int = {
          var tails = 0
          val coins = Array.fill(n + 1)(0)
          for (i <- 1 to n) {
            var k = i
            while (k <= n) {
              coins(k) = (coins(k) + 1) % 2
              k += i
            }
            tails += coins(i)
          }
          tails
        }
      }

      object functional_style {
        import Iterator.iterate

        // much slower because vector
        def countTails(n: Int): Int = {
          val (tails, _) = (1 to n)
            .foldLeft((0, Vector.fill(n + 1)(0))) { case ((tails, coins), i) =>
              val flippedCoins =
                iterate(i)(_ + i).takeWhile(_ <= n).foldLeft(coins) { (coins, k) =>
                  coins.updated(k, (coins(k) + 1) % 2)
                }
              (tails + flippedCoins(i), flippedCoins)
            }
          tails
        }
      }
    }

    object log_n { // O(log n)
      def countTails(n: Int): Int =
        if (n < 2) n
        else {
          val small = countTails(n >> 2) << 1
          val large = small + 1
          if (large * large > n) small
          else large
        }
    }

    object constant { // O(1)
      def countTails(n: Int): Int =
        math.sqrt(n.toDouble).toInt
    }
  }
}
