package com.jtmconsultancy.eco

import org.scalatest._


class EcoTest extends FlatSpec {

 "HandleBin with argument " should "result in 3 bins " in {
   val handleBin = new HandleBin()
   assert (handleBin.bins("1 2 3 4 5 6 7 8 9").size == 3)
 }

}
