package recfun

object Main extends App {
  override def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    //print(balance("(if (zero? x) max (/ 1 x))".toList))
    //print(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    //print(balance(":-)".toList))
    //print(balance("())(".toList))
    
    //print(countChange(4,List(1,2)))
    //print(countChange(300,List(5,10,20,50,100,200,500)))
    //print(countChange(301,List(5,10,20,50,100,200,500)))
    print(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def impl(chars: List[Char], open: Int): Boolean = {
        if (chars.isEmpty) 
          if (open == 0) true
          else false
        else if (chars.head == '(') impl(chars.tail, open + 1)
        else if (chars.head == ')')
          if (open == 0) false
          else impl(chars.tail, open-1)
        else impl(chars.tail, open)
      }
      
      impl(chars, 0)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money <= 0) 0
      else {
        if (!coins.isEmpty) {
          if (money > coins.head) countChange(money - coins.head, coins) + countChange(money, coins.tail)
          else if (money == coins.head) 1 + countChange(money, coins.tail)
          else countChange(money, coins.tail)
        }
        else 0
      }
  }
}
