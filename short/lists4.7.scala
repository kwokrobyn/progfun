object lists {
    def isort(xs: List[Int]): List[Int] = xs match {
        case List() => List()
        case y :: ys => insert(y, isort(ys))
    }

    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
        case List() => List(x)
        case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }

    val x = List(2, 3, 9)
    val y = 7 
    insert(y, x)

    val a = List(7, 3, 9, 2)
    isort(a)

}