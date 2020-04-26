package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1,r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def acc(x: List[Char], current: Int): Boolean = {
      if (current < 0) false // catch illegal close paren 
      else if (x.length == 0) (current == 0) // base case 
      else if (x.head == '(') acc(x.tail, current + 1)
      else if (x.head == ')') acc(x.tail, current - 1)
      else acc(x.tail, current)
    }
    acc(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.length == 0) 0
    else countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
}
