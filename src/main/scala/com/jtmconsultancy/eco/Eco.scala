package com.jtmconsultancy.eco


object Eco {

  def main(args:Array[String]) {
    if (args.length == 0) {
      println("provide the bottles...")
    } else {
      new HandleBin().start(args(0))
    }
  }
}


class HandleBin {


  val binPerms = "BGC".toList.permutations.toList

  def calculate(bins:List[Bin]) =  binPerms.foldLeft(List[ResultPerPermutation]()) {
    (results:List[ResultPerPermutation], perm:List[Char]) => 
    results ::: List(ResultPerPermutation(tidyBinName(perm),countPerPermutation(perm, bins)))
  }

  def countPerPermutation(perm: List[Char], bins: List[Bin]): Int = perm.foldLeft(0) { (sum, char) =>
        // we want the count for a bin
        val bin = bins(perm.indexOf(char))

        sum + countPerBin(bin,char)
  }

  /**
  * results in the amount of bottles that have to be removed for this bin. So if color is brown, we need to now the count of total green
  * and clear bottles.
  */
  private def countPerBin(b:Bin,c:Char):Int =  b.count(c)

  /*
  * makes a nice formatted name of a color permutation
  */
  private def tidyBinName(l:List[Char]) = l.mkString

  def orderResults(results:List[ResultPerPermutation]) = results.sortBy(r => (r.moves,r.binOrder))

  def start(input:String) = {
   val binList = bins(input)

   val optimum = orderResults(calculate(binList)).head
   
   println(optimum.binOrder +" "+optimum.moves)
  }

  /**
  * input string is something like '1 2 3 4 5 6 7 8 9', where each group of 3 represents in order brown, green and clear bottles.
  * so we need a list of 3 groups (denoting the 3 bins) where in each bin the numbers represent the corresponding bottles. 
  */
  def createIntArraysFrom(input: String): List[Array[Int]] = {
    val numbers = input.split(" ").map(_.toInt)
    numbers.grouped(3).toList
  }

  def bins(input:String):List[Bin] = {
    val numbersPerThree = createIntArraysFrom(input)

    numbersPerThree.zipWithIndex.foldLeft(List[Bin]()) {
      (binList, item) =>
        binList ::: List(Bin(item._2 + 1, item._1.toList))
    }
  }
}


case class Bin(number:Int, l:List[Int]) {

   val brown = l(0)
   val green = l(1)
   val clear = l(2)

   def count(c:Char) = c match {
     case 'B' => green + clear
     case 'G' => brown + clear
     case 'C' => green + brown
   }

  override def toString = "Bin# %d, brown %d, green %d, clear %s".format(number,brown, green,clear)
}


case class ResultPerPermutation(binOrder:String, moves:Int)
