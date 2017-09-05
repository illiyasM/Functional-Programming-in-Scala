package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def fact(n: Int): Int = {
      def loop(acc: Int, n: Int): Int =
        if (n == 0) acc
        else loop(acc * n, n - 1)
      loop(1, n)
    }
    fact(r) / (fact(c) * fact(r - c))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def compare(chars: List[Char], accu: Int): Boolean = {
      if (chars.isEmpty) {
        accu == 0
      }
      else {
        val result = {
          if (chars.head == '(') {
            accu + 1
          }
          else if (chars.head == ')') {
            accu - 1
          }
          else {
            accu
          }
        }
        if (result >= 0) compare(chars.tail, result)
        else false
      }
    }
    compare(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
