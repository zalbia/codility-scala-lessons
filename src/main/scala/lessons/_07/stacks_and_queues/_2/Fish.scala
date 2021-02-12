package lessons._07.stacks_and_queues._2

// N is an integer within the range [1..100,000];
// each element of array A is an integer within the range [0..1,000,000,000];
// each element of array B is an integer that can have one of the following values: 0, 1;
// the elements of A are all distinct.
object Solution {
  //
  def solution(a: Array[Int], b: Array[Int]): Int = { // 100%| T: O(n) | S: O(n)
    a.iterator.zip(Going.fromZerosAndOnes(b.iterator)).foldLeft(Survivors()) {
      case (survivors@Survivors(Nil, _), (_, Going.Upstream)) =>
        survivors.incrementUpstream()
      case (survivors, (fish, Going.Downstream)) =>
        survivors.addDownstream(fish)
      case (survivors, (fish, Going.Upstream)) =>
        survivors.tryToEat(fish)
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
    def tryToEat(upStreamFish: Upstream) = {
      val downStreamSurvivors = downStream.dropWhile(_ < upStreamFish) // RIP & TEAR
      copy(downStreamSurvivors, if (downStreamSurvivors.isEmpty) upstream + 1 else upstream)
    }
  }

}
