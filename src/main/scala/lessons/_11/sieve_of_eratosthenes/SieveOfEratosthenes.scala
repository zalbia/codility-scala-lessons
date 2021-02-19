package lessons._11.sieve_of_eratosthenes

object SieveOfEratosthenes {

  object sieve_of_eratosthenes {

    object procedural_style {
      def sieve(n: Int): Array[Boolean] = {
        val sieve = Array.fill(n + 1)(true)
        sieve(0) = false
        sieve(1) = false
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
  }
}
