package lessons._11.sieve_of_eratosthenes

import scala.collection.mutable.ArrayBuffer

object SieveOfEratosthenes {

  object sieve_of_eratosthenes { // T: O(n log log n) | S: O(n)

    object procedural_style {
      def sieve(n: Int): Array[Boolean] = {
        val sieve = Array(false, false) ++ Array.fill(n - 1)(true)
        var i     = 2
        while (i * i <= n) {
          if (sieve(i)) {
            var k = i * i
            while (k <= n) {
              sieve(k) = false
              k += i
            }
          }
          i += 1
        }
        sieve
      }
    }

    object functional_style {
      import Iterator.iterate

      // unfold then fold (hylomorphism)
      def sieve(n: Int): Vector[Boolean] = {
        val sieve = Vector(false, false) ++ Vector.fill(n - 1)(true)
        iterate(2)(_ + 1).takeWhile(i => i * i <= n).foldLeft(sieve) { case (sieve, i) =>
          if (sieve(i)) iterate(i * i)(_ + i).takeWhile(_ <= n).foldLeft(sieve)(_.updated(_, false))
          else sieve
        }
      }
    }
  }

  object factorization {

    object procedural_style {
      def mkSmallestPrimeSieve(n: Int): Array[Int] = { // T: O(n) | S: O(n)
        val sieve = Array.ofDim[Int](n + 1)
        var i     = 2
        while (i * i <= n) {
          if (sieve(i) == 0) {
            var k = i * i
            while (k <= n) {
              if (sieve(k) == 0)
                sieve(k) = i
              k += i
            }
          }
          i += 1
        }
        sieve
      }

      def factorize(n: Int)(sieve: Array[Int]): Array[Int] = {  // T: O(log n) | S: O(1)
        val primeFactors = new ArrayBuffer[Int]()
        var x            = n
        while (sieve(x) > 0) {
          primeFactors += sieve(x)
          x /= sieve(x)
        }
        primeFactors += x
        primeFactors.toArray
      }
    }

    object iterator_foreach {
      import Iterator.{ from, iterate }

      def mkSmallestPrimeSieve(n: Int): Array[Int] = {
        val sieve = Array.ofDim[Int](n + 1)
        for {
          i <- from(2).takeWhile(i => i * i <= n) if sieve(i) == 0
          k <- iterate(i * i)(_ + i).takeWhile(_ <= n) if sieve(k) == 0
        } sieve(k) = i
        sieve
      }

      def factorize(n: Int)(sieve: Array[Int]): Array[Int] = {
        val primeFactors = new ArrayBuffer[Int]()
        var x            = n
        for (n <- iterate(n)(x => x / sieve(x)).takeWhile(sieve(_) > 0)) {
          primeFactors += sieve(n)
          x = n
        }
        (primeFactors += x / primeFactors.last).toArray
      }
    }

    object functional_style {
      import Iterator.{ from, iterate }

      // cleaner way to do this would be with a state monad
      def mkSmallestPrimeSieve(n: Int): Vector[Int] =
        from(2)
          .takeWhile(i => i * i <= n)
          .foldLeft(Vector.fill(n + 1)(0)) { (sieve, i) =>
            if (sieve(i) == 0)
              iterate(i * i)(_ + i)
                .takeWhile(_ <= n)
                .foldLeft(sieve) { (sieve, k) =>
                  if (sieve(k) == 0) sieve.updated(k, i)
                  else sieve
                }
            else sieve
          }

      def factorize(n: Int)(sieve: Array[Int]): Array[Int] = {
        iterate(n)(x => x / sieve(x))
          .takeWhile(sieve(_) > 0)
          .foldLeft(List.empty[Int]) { (primeFactors, x) =>
            val next = x / sieve(x)
            if (sieve(next) == 0) next :: sieve(x) :: primeFactors
            else sieve(x) :: primeFactors
          }.toArray
      }
    }
  }
}
