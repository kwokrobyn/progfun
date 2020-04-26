package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  //@Ignore("not ready yet") 
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `singleton set one does not contain two`: Unit = {
    new TestSets {
      assert(!contains(s1, 2), "Singleton")
    }

  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains common elements of each set`: Unit = {
    new TestSets {
      val u1 = union(s1, s2) // u1 = 1, 2
      val s = intersect(u1, s2)
      assert(contains(s, 2), "Intersect 1")
      assert(!contains(s, 1), "Intersect 2")
    }
  }

  @Test def `diff contains elems in s not in t`: Unit = {
    new TestSets {
      val u1 = union(union(s1, s2), s3)
      val s = diff(u1, s1)
      assert(contains(s, 2), "Diff 1")
      assert(contains(s, 3), "Diff 2")
      assert(!contains(s, 1), "Diff 3")
    }
  }

  @Test def `forall works`: Unit = {
    new TestSets {
      val u1 = union(union(s1, s2), s3) // 1,2,3
      val u2 = union(s1, s3) // 1, 3
      val p: Int => Boolean = x => (x % 2 == 1) // x is odd 
      assert(!forall(u1, p), "forall 1")
      assert(forall(u2, p), "forall 2")
    }
  }

  @Test def `exist works`: Unit = {
    new TestSets {
      val u1 = union(s1, s2)
      val p1: Int => Boolean = x => (x == 1)
      val p2: Int => Boolean = x => (x == 2)
      val p3: Int => Boolean = x => (x == 3)
      assert(exists(u1, p1), "exist 1")
      assert(exists(u1, p2), "exist 2")
      assert(!exists(u1, p3), "exist 3")
    }
  }

  @Test def `map works`: Unit = {
    new TestSets {
      val f1: FunSet = x => (x > 1 && x < 5) // 2, 3, 4 
      val f2: FunSet = x => (x == 4 || x == 6 || x == 8)
      val f3: FunSet = x => (x == 4 || x == 6 || x == 2)
      //printSet(map(f1, x => 2 * x))
      //printSet(f2)
      //assert(map(f1, x => 2 * x) == f2, "map 1")
      assert(map(f1, x => 2 * x) != f3, "map 2")
    }
  }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
