package com.jtmconsultancy.eco

import scala.util._


object Eco {

  def main(args:Array[String]) {
   val handleBin = new HandleBin()

   val parseInput =  Try{

     val numbers = inputToIntList(args)
     if (numbers.size != 9) throw new IllegalArgumentException("wrong length")
     intsGroupedPerThree(numbers)

   }

   parseInput match {
       case Failure(_) => println("provide 9 bottles...")
       case Success(numbersPerThree)  => handleBin.start(numbersPerThree)
    }
  }


  def intsGroupedPerThree(numbers: Array[Int]): List[Array[Int]] = {
    numbers.grouped(3).toList
  }

  def inputToIntList(args: Array[String]): Array[Int] = {
    args(0).split(" ").map(_.toInt)
  }
}



class HandleBin {

  val binPermutations = "BGC".toList.permutations.toList

  def calculate(bins:List[Bin]) =  binPermutations.foldLeft(List[ResultPerPermutation]()) {
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

  private def tidyBinName(l:List[Char]) = l.mkString

  def orderResults(results:List[ResultPerPermutation]) = results.sortBy(r => (r.moves,r.binOrder))

  def start(numbersPerThree: List[Array[Int]])  {

   val binList = bins(numbersPerThree)
   displayResult(optimumBinOrder(binList))
   
  }

  def optimumBinOrder(binList: List[Bin]): ResultPerPermutation = {
    orderResults(calculate(binList)).head
  }

  def displayResult(optimum:ResultPerPermutation) {

    println(optimum.binOrder +" "+optimum.moves)

  }

  def bins(numbersPerThree: List[Array[Int]]):List[Bin] = {
    
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
