import scala.io.Source

object question9 extends App {
  def toInt(in: String): Int = in.toInt
  val program = Source.fromFile("/Users/christina/hub/advent2019/input9.txt").getLines.mkString.split(",").map(_.toLong)

  val zeroArray = Array.fill(1000)(0).map(_.toLong)
  val programUpdated = program ++ zeroArray

  val inputVal = 2.toLong

  class Instructions(var ins: Array[Long], var ptr: Int, var relBase: Long){

    def getIndex(offset: Int): Int = {
      val mode = getMode(offset)
      mode match {
        case 2 => {
          ins(ptr + offset).toInt + relBase.toInt
        }
        case _ => {
          ins(ptr + offset).toInt
        }
      }
    }

    def getValAtIndex(offset: Int): Long = {
      val mode = getMode(offset)
      //println("The mode is: "+mode)
      mode match {
        case 0 => {
          val value = ins(ptr + offset)
          ins(value.toInt)
        }
        case 1 =>{
          ins(ptr+offset)
        }
        case 2 => {
          val value = ins(ptr + offset)
          ins(value.toInt + relBase.toInt)
        }
      }
    }

    def incrementPointer(in: Int): Unit = {
      ptr = ptr + in
    }

    def updateRelBase(in: Long): Unit = {
      relBase = relBase + in
      //println("The relative base is: "+relBase)
    }

    def jumpPointer(in: Int): Unit = {
      ptr = in
    }

    def printInstructions: Unit = {
      println(ins(ptr),ins(ptr+1),ins(ptr+2),ins(ptr+3))
    }

    def getOpcode: Int = {
      val opcodeArray = ins(ptr)
      val opcodeArrayStr = "%05d".format(opcodeArray)
      opcodeArrayStr(3).asDigit match {
        case 0 => {
          opcodeArrayStr(4).asDigit
        }
        case 9 => {
          99
        }
      }
    }

    def getMode(offset: Int): Int = {
      val opcodeArray = ins(ptr)
      val opcodeArrayStr = "%05d".format(opcodeArray)
      val index = 3 - offset
      opcodeArrayStr(index).asDigit
    }

    def printState: Unit = {
      //println("The value at index 0 is: "+ins(0))
    }

    def updateState(updateIndex: Int, newVal: Long):Unit = {
      ins = ins.updated(updateIndex, newVal)
    }

    def updateInstructions(opcode: Long ):Unit = {

      opcode match {
        case 1 => {
          val val3 = getIndex(3)
          val newVal =  getValAtIndex(1) + getValAtIndex(2)
          updateState(val3, newVal)
          //println("Setting location "+val3+" to "+newVal)
          incrementPointer(4)
        }
        case 2 => {
          val val3 = getIndex(3)
          val newVal = getValAtIndex(1) * getValAtIndex(2)
          updateState(val3, newVal)
          //println("Setting location "+val3+" to "+newVal)
          incrementPointer(4)
        }
        //Opcode 3 takes a single integer as input and saves it to the position given by its
        // only parameter. For example, the instruction 3,50 would take an input value and
        // store it at address 50.
        case 3 => {
          val updateIndex = getIndex(1)
          updateState(updateIndex, inputVal)
          println("The relative base is "+relBase)
          println("Setting location "+updateIndex+" to "+inputVal)
          println(ins(1000))
          incrementPointer(2)
        }
        //Opcode 4 outputs the value of its only parameter.
        // For example, the instruction 4,50 would output the value at address 50.
        case 4 => {
          val outputVal = getValAtIndex(1)
          println("The output is "+outputVal)
          incrementPointer(2)
        }
        //Opcode 5 is jump-if-true: if the first parameter is non-zero,
        // it sets the instruction pointer to the value from the second parameter.
        // Otherwise, it does nothing.
        case 5 => {
          if (getValAtIndex(1) != 0) {
            jumpPointer(getValAtIndex(2).toInt)
          }
          else {
            incrementPointer(3)
          }
        }
        //Opcode 6 is jump-if-false: if the first parameter is zero,
        // it sets the instruction pointer to the value from the second parameter.
        // Otherwise, it does nothing.
        case 6 => {
          if (getValAtIndex(1) == 0) {
            jumpPointer(getValAtIndex(2).toInt)
          }
          else {
            incrementPointer(3)
          }
        }
        //Opcode 7 is less than: if the first parameter
        // is less than the second parameter, it stores 1 in the position
        // given by the third parameter. Otherwise, it stores 0.
        case 7 => {
          val updateIndex = getIndex(3)
          if (getValAtIndex(1) < getValAtIndex(2)) {
            updateState(updateIndex, 1)
            //println("Setting location "+updateIndex+" to "+1)
          }
          else {
            updateState(updateIndex, 0)
            //println("Setting location "+updateIndex+" to "+0)
          }
          incrementPointer(4)
        }
        //Opcode 8 is equals: if the first parameter is equal to the second parameter,
        // it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 8 => {
          val updateIndex = getIndex(3)
          if (getValAtIndex(1) == getValAtIndex(2)) {
            updateState(updateIndex, 1)
            //println("Setting location "+updateIndex+" to "+1)
          }
          else {
            updateState(updateIndex, 0)
            //println("Setting location "+updateIndex+" to "+0)
          }
          incrementPointer(4)
        }
        //Opcode 9 adjusts the relative base by the value of its only parameter.
        // The relative base increases (or decreases, if the value is negative)
        // by the value of the parameter.
        case 9 => {
          updateRelBase(getValAtIndex(1))
          incrementPointer(2)
        }
      }
    }
  }

  val instructions = new Instructions(programUpdated, 0, 0)

  @scala.annotation.tailrec
  def getInstructions:Unit = {
    //println("The opcode is: "+instructions.getOpcode)
    //println(instructions.ins.mkString(" "))

    //instructions.printInstructions

    if (instructions.getOpcode == 99) {
      instructions.printState
    }
    else {
      instructions.updateInstructions(instructions.getOpcode)
      getInstructions
    }
  }

  getInstructions

}
