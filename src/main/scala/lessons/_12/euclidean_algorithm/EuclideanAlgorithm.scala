package lessons._12.euclidean_algorithm

import scala.annotation.tailrec

object EuclideanAlgorithm {

  object by_subtraction { // T: O(n) | S: O(1)
    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt =
      if (a == b)
        a
      else if (a > b)
        gcd(a - b, a)
      else
        gcd(a, b - a)
  }

  object by_division { // T: O(log(a + b)) | S: O(1)
    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt =
      if (a % b == 0) b else gcd(b, a % b)
  }

  object binary_divide_and_conquer {
    def gcd(a: BigInt, b: BigInt): BigInt = {
      @tailrec
      def loop(a: BigInt, b: BigInt, res: BigInt): BigInt =
        if (a == b)
          res * a
        else if (a % 2 == 0 && b % 2 == 0)
          loop(a / 2, b / 2, 2 * res)
        else if (a % 2 == 0)
          loop(a / 2, b, res)
        else if (b % 2 == 0)
          loop(a, b / 2, res)
        else if (a > b)
          loop(a - b, b, res)
        else
          loop(a, b - a, res)
      loop(a, b, 1)
    }
  }

  object least_common_multiple {

    def lcm(a: BigInt, b: BigInt)(gcd: (BigInt, BigInt) => BigInt): BigInt =
      a * b / gcd(a, b)

    object of_many_nums {

      object unsafe_recursive {
        def lcms(nums: List[BigInt])(gcd: (BigInt, BigInt) => BigInt): BigInt = nums match {
          case Nil          => 1
          case head :: tail => lcm(head, lcms(tail)(gcd))(gcd)
        }
      }

      object fold_right {
        def lcms(nums: Iterable[BigInt])(gcd: (BigInt, BigInt) => BigInt): BigInt =
          nums.reduceRight((a, b) => lcm(a, b)(gcd))
      }
    }
  }
}
