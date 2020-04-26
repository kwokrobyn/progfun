package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty), 
    for {
      x <- arbitrary[A]
      h <- oneOf(genHeap, const(empty))
    } yield insert(x, h)
  )

  def isEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true 
      else if (isEmpty(h1) && !isEmpty(h2)) false 
      else if (isEmpty(h2) && !isEmpty(h1)) false 
      else (findMin(h1) == findMin(h2) && isEqual(deleteMin(h1), deleteMin(h2)))
    }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min single elem") = forAll { (x: Int) => 
    val h = insert(x, empty)
    findMin(h) == x
  }

  property("min of 2 elems") = forAll { (x: Int, y: Int) =>
    findMin(insert(y, insert(x, empty))) == min(x, y)
  }

  property("insert and delete") = forAll { (x: Int) =>
    isEmpty(deleteMin(insert(x, empty))) 
  }

  property("sorted list") = forAll { (h: H) =>
    def isSorted(minVal: Int, h: H): Boolean = {
      if (isEmpty(h)) true 
      else {
        val m = findMin(h)
        (m >= minVal && isSorted(m, deleteMin(h)))
      }
    }
    isSorted(Int.MinValue, h)
  }

  property("meld test") = forAll { (h1: H, h2: H) => 
    if (isEmpty(h1) && isEmpty(h2)) true 
    else {
       val t = findMin(meld(h1, h2))
      if (isEmpty(h1)) t == findMin(h2)
      else if (isEmpty(h2)) t == findMin(h1)
      else (t == findMin(h1) || t == findMin(h2))
    }
  }

  property("meld equality test") = forAll { (h1: H, h2: H) =>
    isEqual(meld(h1, empty), h1) && isEqual(meld(h2, empty), h2)
  }

  property("meld equality test 2") = forAll { (h1: H, h2: H) =>
    isEqual(meld(h1, h2), meld(h2, h1))
  }

  property("meld equality test 2") = forAll { (h: H, x: Int, y: Int) =>
    val m = insert(y, insert(x, empty))
    isEqual(meld(h, m), insert(y, insert(x, h)))
  }
}
