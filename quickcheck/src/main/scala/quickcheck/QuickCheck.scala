package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h1: H, h2: H) =>
    def unheap(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h) :: unheap(deleteMin(h))
    unheap(meld(h1, h2)) == (unheap(h1) ++ unheap(h2)).sorted
  }
}
