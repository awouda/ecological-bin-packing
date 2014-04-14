package com.jtmconsultancy.eco

import org.scalatest._


class EcoTest extends FlatSpec {

 "Eco with argument \"1 2 3 4 5 6 7 8 9\"  " should "result in 3 arrays " in {

   assert (Eco.intsGroupedPerThree(Eco.inputToIntList(Array("1 2 3 4 5 6 7 8 9"))).size == 3)
 }

  "HandleBin with argument \"1 2 3 4 5 6 7 8 9\"  " should "result BCG with 30 moves " in {
     val groupedBottles = Eco.intsGroupedPerThree(Eco.inputToIntList(Array("1 2 3 4 5 6 7 8 9")))
     val handleBin = new HandleBin()
     val binList = handleBin.bins(groupedBottles)
     val result = handleBin.optimumBinOrder(binList)
     assert (result.binOrder == "BCG")
     assert (result.moves == 30)

  }

  "HandleBin with argument \"123 654 789 963 258 741 159 963 357\"  " should "result CBG with 2292 moves " in {
    val groupedBottles = Eco.intsGroupedPerThree(Eco.inputToIntList(Array("123 654 789 963 258 741 159 963 357")))
    val handleBin = new HandleBin()
    val binList = handleBin.bins(groupedBottles)
    val result = handleBin.optimumBinOrder(binList)
    assert (result.binOrder == "CBG")
    assert (result.moves == 2292)

  }









}
