package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    el <- arbitrary[A]
    heap <- frequency((1, empty), (10, genHeap))
  } yield insert(el, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(x: H): List[A] = {
    if (isEmpty(x)) Nil
    else findMin(x) :: toList(deleteMin(x))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("melding") = forAll { (hA: H, hB: H) =>
    Set(findMin(hA), findMin(hB)).contains(findMin(meld(hA, hB)))
  }

  property("sorted min") = forAll {h: H =>
    def checkSort(list: List[A]): Boolean = {
      list match {
        case Nil => true
        case head :: Nil => true
        case head :: second :: rest => (head <= second) && checkSort(second :: rest)
      }
    }

    checkSort(toList(h))
  }

  property("insert list and compare sorted") = forAll { list: List[Int] =>
    def insertList(list: List[Int], acc: H): H = list match {
      case Nil => acc
      case a :: rest => insertList(rest, insert(a, acc))
    }

    list.sortWith((x: Int, y: Int) => (x <= y)) == toList(insertList(list, empty))
  }

}
