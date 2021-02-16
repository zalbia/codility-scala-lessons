package lessons._07.stacks_and_queues

import scala.reflect.ClassTag

object StacksAndQueues {

  object list_stacks {

    object mutable {
      class Stack[A](var list: List[A]) {

        def push(a: A): Unit =
          list = a :: list

        def unsafePeek: A =
          list.head

        def peek: Option[A] =
          list.headOption

        def unsafePop(): A = {
          val top = list.head
          list = list.tail
          top
        }

        def pop(): Option[A] =
          list match {
            case Nil         =>
              None
            case top :: rest =>
              list = rest
              Some(top)
          }
      }
    }

    object immutable {
      implicit class StackOps[A](val list: List[A]) {
        def push(a: A): List[A] =
          a :: list

        def peek: Option[A] =
          list.headOption

        def pop: (List[A], Option[A]) =
          list match {
            case Nil         => (list, None)
            case top :: rest => (rest, Some(top))
          }
      }
    }
  }

  object queue {
    class CyclicArrayQueue[A] private (var array: Array[Option[A]]) {
      var head, tail = 0

      def push(a: A): Unit = {
        tail = (tail + 1) % array.length
        array(tail) = Some(a)
      }

      def pop(): Option[A] = {
        head = (head + 1) % array.length
        array(head)
      }

      def size: Int =
        (tail - head + array.length) % array.length

      def isEmpty: Boolean =
        head == tail
    }

    object CyclicArrayQueue {
      def apply[A](bufferSize: Int): CyclicArrayQueue[A] =
        new CyclicArrayQueue[A](Array.fill(bufferSize)(None))
    }

    case class CyclicVectorQueue[A] private (vector: Vector[Option[A]], head: Int = 0, tail: Int = 0) {

      def push(a: A): CyclicVectorQueue[A] =
        copy(tail = (tail + 1) % vector.length, vector = vector.updated(tail, Some(a)))

      def pop: (CyclicVectorQueue[A], Option[A]) = {
        val newHead = (head + 1) % vector.length
        (copy(head = newHead), vector(newHead))
      }

      def size: Int =
        (tail - head + vector.length) % vector.length

      def isEmpty: Boolean =
        head == tail
    }

    object CyclicVectorQueue {
      def apply[A](bufferSize: Int): CyclicVectorQueue[A] =
        new CyclicVectorQueue[A](Vector.fill(bufferSize)(None))
    }
  }

  object grocery_store {
    object procedural_style {
      def groceryStore(a: Array[Int]) = {
        var size, minPeople = 0
        for (elem <- a)
          if (elem == 0)
            size += 1
          else {
            size -= 1
            minPeople = math.max(minPeople, -size)
          }
        minPeople
      }
    }

    object functional_style {
      def groceryStore(a: Array[Int]) = {
        val (minPeople, _) = a.foldLeft((0, 0)) { case ((minPeople, size), elem) =>
          if (elem == 0)
            (minPeople, size + 1)
          else {
            val newSize = size - 1
            (math.max(minPeople, -newSize), newSize)
          }
        }
        minPeople
      }
    }
  }
}
