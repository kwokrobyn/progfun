// 1.3 
object generators {
    val integers = new Generator[Int] {
        def generate = scala.util.Random.nextInt()
    }

    val booleans = integers map (_ >= 0)

    trait Tree 

    case class Inner(left: Tree, right: Tree) extends Tree 

    case class Leaf(x: Int) extends Tree 

    def trees: Generator[Tree] = for {
        isLeaf <- booleans 
        tree <- if (isLeaf) leafs else inners
    }

    def leafs: Generator[Leaf] = for {
        x <- integers 
    } yield Leaf(x)

    def inners: Generator[Inner] for {
        left <- trees 
        right <- trees 
    } yield Inner(left, right)

    // Testing 
    // takes a test function that returns true or false
    def test[T](g: Generator[T], numTimes: Int = 100)
        (test: T => Boolean): Unit = {
            for (i <- 0 until numTimes) {
                val value = g.generate 
                assert(test(value), "test failed for " + value)
            }
            println("passed "+numTimes+" tests")
        }

}