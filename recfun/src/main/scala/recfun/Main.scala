package recfun

import scala.annotation.tailrec

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
    if (c == 0 || c == r) 1
    else if (c == 1) r
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(acc: Int, chars: List[Char]): Boolean =
      chars match {
        case Nil       => acc == 0
        case '(' :: xs => loop(acc + 1, xs)
        case ')' :: xs => if (acc < 1) false else loop(acc - 1, xs)
        case _ :: xs   => loop(acc, xs)
      }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0 || coins.isEmpty) 0
    else coins.filter(_ <= money)
      .map { coin =>
        val newMoneyToCalc = money - coin
        if (newMoneyToCalc == 0) 1
        else countChange(newMoneyToCalc, coins.filter(_ >= coin))
      }
      .sum

  }
}
