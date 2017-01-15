package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def isBalanced(index: Int, acc: Int): Int = {
      if (index == chars.length) acc
      else {
        val chr = chars(index)
        if (chr == '(') isBalanced(index+1, acc+1)
        else if (chr == ')') {
          if (acc > 0) isBalanced(index + 1, acc - 1)
          else -1
        }
        else isBalanced(index+1, acc)
      }
    }
    isBalanced(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def isBalanced(index: Int, ub: Int, acc: Int): Int = {
      if (index == ub) acc
      else {
        val chr = chars(index)
        if (chr == '(') isBalanced(index+1, ub, acc+1)
        else if (chr == ')') {
          if (acc > 0) isBalanced(index + 1, ub, acc - 1)
          else -1
        }
        else isBalanced(index+1, ub, acc)
      }
    }

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): Int = {
      //arg1 and arg2 are both mid, this is probably actually for
      //4 part parallel whereas I'm only using 2 parts
      val p = parallel(reduce(idx, arg1), reduce(arg2, until))
      p._1 + p._2
    }

    def reduce(from: Int, until: Int): Int = {
      //val mid = math.floor((until - from)/2).toInt + from
      val s = until - from
      if (s > threshold) {
        val mid = math.floor(s/2).toInt
        traverse(from, until, from + mid, from + mid)
      } else {
        isBalanced(from, until, 0)
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
