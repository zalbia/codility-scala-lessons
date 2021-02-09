package lessons._07.stacks_and_queues._2

object Solution {
  // N is an integer within the range [1..100,000];
  // each element of array A is an integer within the range [0..1,000,000,000];
  // each element of array B is an integer that can have one of the following values: 0, 1;
  // the elements of A are all distinct.
  def solution(a: Array[Int], b: Array[Int]): Int = { // 100% O(n)
    a.iterator.zip(Going.fromZerosAndOnes(b.iterator)).foldLeft(Survivors()) {
      case (survivors@Survivors(Nil, _), (_, Going.Upstream)) =>
        survivors.incrementUpstream()
      case (survivors, (fish, Going.Downstream)) =>
        survivors.addDownstream(fish)
      case (survivors, (fish, Going.Upstream)) =>
        survivors.meet(fish)
    }.total
  }

  type Upstream = Int
  type Downstream = List[Int]

  sealed trait Going
  object Going {
    case object Downstream
    case object Upstream
    def fromZerosAndOnes(nums: Iterator[Int]) = nums.map {
      case 0 => Upstream
      case 1 => Downstream
    }
  }

  case class Survivors(downStream: Downstream = List.empty, upstream: Upstream = 0) {
    def addDownstream(fish: Int) = copy(fish :: downStream)
    def incrementUpstream() = copy(upstream = upstream + 1)
    lazy val total = downStream.size + upstream
    def meet(upStreamFish: Upstream) = {
      val downStreamSurvivors = downStream.dropWhile(_ < upStreamFish)
      copy(downStreamSurvivors, if (downStreamSurvivors.isEmpty) upstream + 1 else upstream)
    }
  }

}

import utest._

import scala.util.Random

object FishTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val extreme = 10000 to 1000000000 by 10000
  val reverseExtreme = extreme.reverse

  val tests = Tests {
    test { check(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 0, 0), 2) }
    test { check(Array(1), Array(1), 1) }
    test { check(Array(1), Array(0), 1) }
    test { check(Array(1), Array(1), 1) }
    test { check(Array(1, 2), Array(0, 0), 2) }
    test { check(Array(1, 2), Array(0, 0), 2) }
    test { check(Array(1, 2), Array(1, 0), 1) }
    test { check(Array(1, 2), Array(0, 1), 2) }
    test { check(Array(1, 2), Array(1, 1), 2) }
    test { check(Array(2, 1), Array(0, 0), 2) }
    test { check(Array(2, 1), Array(0, 0), 2) }
    test { check(Array(2, 1), Array(1, 0), 1) }
    test { check(Array(2, 1), Array(0, 1), 2) }
    test { check(Array(2, 1), Array(1, 1), 2) }
    test { check(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 1, 0), 2) }
    test { check(extreme.toArray, Array(1) ++ Array.fill(99999)(0), 99999) }
    test { check(reverseExtreme.toArray, Array(1) ++ Array.fill(99999)(0), 1) }
    test { f(Random.shuffle[Int, IndexedSeq](extreme).toArray, Array.fill(100000)(random.nextInt(2))) }
  }

  def check(a: Array[Int], b: Array[Int], expected: Int): Unit = {
    val result = f(a, b)
    assert(result == expected)
  }
}
