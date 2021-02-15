package lessons._08.leader

object Leader {
  type Elem  = Int
  type Count = Int

  object slow_leader {

    object array { // T: O(n^2) | S: O(n)
      def slowLeader(a: Array[Int]): Int = {
        var leader = -1
        for (candidate <- a)
          if (a.count(_ == candidate) > a.length / 2)
            leader = candidate
        leader
      }
    }

    object map { // slow because Map
      def slowLeader(a: Array[Int]): Int = // T: O(n * eC) | S: O(n)
        if (a.isEmpty) -1
        else {
          a.foldLeft(Map.empty[Elem, Count]) { (counts, elem) =>
            counts + (elem -> counts.get(elem).map(_ + 1).getOrElse(1))
          }.maxBy(_._2) match {
            case (_, count) if count <= a.length / 2 => -1
            case (elem, _)                           => elem
          }
        }
    }
  }

  // code practically speaks for itself
  def fastLeader(a: Array[Int]): Int = // T: O(n log n) | S: O(n)
    if (a.isEmpty) -1
    else {
      val candidate = a.sorted.apply(a.length / 2)
      if (a.count(_ == candidate) > a.length / 2) candidate else -1
    }

  object golden_leader {
    object procedural {
      def goldenLeader(a: Array[Int]): Int = {
        var size  = 0
        var value = null.asInstanceOf[Int]
        for (elem <- a)
          if (size == 0) {
            size += 1
            value = elem
          } else if (value != elem)
            size -= 1
          else
            size += 1
        var candidate = -1
        if (size > 0)
          candidate = value
        var leader    = -1
        val count     = a.count(_ == candidate)
        if (count > a.length / 2)
          leader = candidate
        leader
      }
    }

    // a leader should outnumber the occurrences of every other element
    object stack { // stack can contain the candidate at the end if there is any
      def goldenLeader(a: Array[Int]): Int = { // T: O(n) | S: O(n)
        val stack = a.foldLeft(List.empty[Int]) {
          case (Nil, elem)                             => elem :: Nil   // push
          case (stack @ top :: _, elem) if top == elem => elem :: stack // peek & push
          case (_ :: pop, _)                           => pop
        }
        stack.headOption.filter(c => a.count(_ == c) > a.length / 2).getOrElse(-1)
      }
    }

    object constant_space {
      // a leader should outnumber the occurrences of every other element
      // you're left with a size and the mode
      // reduce the stack to just a size and the top element
      def goldenLeader(a: Array[Int]): Int = { // T: O(n) | S: O(1)
        val (size, mode) = a.foldLeft((0, null.asInstanceOf[Int])) { // initial value doesn't matter
          case ((0, _), elem)                         => (1, elem)
          case ((size, value), elem) if value == elem => (size + 1, value)
          case ((size, value), _)                     => (size - 1, value)
        }
        val candidate    = if (size > 0) Some(mode) else None // stack headOption
        candidate.filter(c => a.count(_ == c) > a.length / 2).getOrElse(-1)
      }
    }
  }
}
