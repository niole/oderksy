package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    /*
     dividing the height of the terrain xs(i) with the distance from the viewing point i

      if the viewing angle of some point B is lower than the viewing angle of an earlier
      point A, then the point B is not visible
     */
    def writeToOuput(index: Int, ub: Int, max: Float): Unit = {
      if (index < ub) {
        val nextTangent = input(index) / index
        val nextMax = if (nextTangent >= max) nextTangent else max
        output(index) = nextMax
        writeToOuput(index+1, ub, nextMax)
      }
    }
    output(0) = 0
    writeToOuput(1, input.length, 0)
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def findMaxAngle(index: Int, max: Float): Float = {
      if (index == until) max
      else {
        val nextMax = if (index == 0) math.max(0, max) else math.max(input(index)/index, max)
        findMaxAngle(index+1, nextMax)
      }
    }

    findMaxAngle(from, 0)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  var ups = 0
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    val s = end - from
    if (s <= threshold) {
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      ups += 1
      val h = math.floor(s/ 2).toInt
      val trees =
        parallel(upsweep(input, from, from + h, threshold),
          upsweep(input, from + h, end, threshold))
      Node(trees._1, trees._2)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    def writeToOutput(index: Int, max: Float): Unit = {
      if (index < until) {
        val height = input(index) / index
        val nextMax = if (height > max) height else max
        output(index) = nextMax
        writeToOutput(index+1, nextMax)
      }
    }

    writeToOutput(from, startingAngle)
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  var downs = 0
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Leaf(from, end, maxPrev) => downsweepSequential(input, output, startingAngle, from, end)
    case Node(left, right) => {
      val nextMax = tree.maxPrevious
      downs += 1
      parallel(downsweep(input, output, nextMax, left), downsweep(input, output, nextMax, right))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    ups = 0
    val tree = upsweep(input, 0, input.length, threshold)
    var m = Array(1,3,7,5,9, 10, 11, 20, 19).map(x => x.toFloat)
    val t = 3
    val tr = upsweep(m, 0, m.length, t)
    /*println("tr")
    println(tr)

    println("m")
    println(m.mkString(" "))*/
    downs = 0
    downsweep(input, output, tree.maxPrevious, tree)
//    println(ups == downs)
  }
}
