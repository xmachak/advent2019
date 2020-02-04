import scala.io.Source

object question5 extends App {
  def toInt(in: String): Int = in.toInt
  val program = Source.fromFile("/Users/christina/hub/advent2019/input5.txt").getLines.mkString.split(",").map(_.toInt)

  // replace position 1 with the value 12 and replace position 2 with the value 2.
  // val programUpdated = program.updated(1,12).updated(2,2)
  val programUpdated = program

  val inputVal = 5

  class Instructions(var ins: Array[Int], var ptr: Int){

    def getIndex(offset: Int): Int = {
      ins(ptr+offset)
    }

    def getValAtIndex(offset: Int): Int = {
      val mode = getMode(offset)
      println(mode)
      mode match {
        case 0 => {
          val value = ins(ptr + offset)
          ins(value)
        }
        case 1 =>{
          ins(ptr+offset)
        }
      }
    }

    def incrementPointer(in: Int): Unit = {
      ptr = ptr + in
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
      opcodeArrayStr(4).asDigit
    }

    def getMode(offset: Int): Int = {
      val opcodeArray = ins(ptr)
      val opcodeArrayStr = "%05d".format(opcodeArray)
      val index = 3 - offset
      //return index 2 for offset 1
      //return index 1 for offset 2
      //return index 0 for offset 3
      opcodeArrayStr(index).asDigit
    }

    def printState: Unit = {
      println("The value at index 0 is: "+ins(0))
    }

    def updateState(updateIndex: Int, newVal: Int):Unit = {
      ins = ins.updated(updateIndex, newVal)
    }

    def updateInstructions(opcode: Int ):Unit = {

      opcode match {
        case 1 => {
          val val3 = getIndex(3)
          val newVal =  getValAtIndex(1) + getValAtIndex(2)
          updateState(val3, newVal)
          println("Setting location "+val3+" to "+newVal)
          incrementPointer(4)
        }
        case 2 => {
          val val3 = getIndex(3)
          val newVal = getValAtIndex(1) * getValAtIndex(2)
          updateState(val3, newVal)
          println("Setting location "+val3+" to "+newVal)
          incrementPointer(4)
        }
        //Opcode 3 takes a single integer as input and saves it to the position given by its
        // only parameter. For example, the instruction 3,50 would take an input value and
        // store it at address 50.
        case 3 => {
          val updateIndex = getIndex(1)
          updateState(updateIndex, inputVal)
          println("Setting location "+updateIndex+" to "+inputVal)
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
            jumpPointer(getValAtIndex(2))
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
            jumpPointer(getValAtIndex(2))
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
            println("Setting location "+updateIndex+" to "+1)
          }
          else {
            updateState(updateIndex, 0)
            println("Setting location "+updateIndex+" to "+0)
          }
          incrementPointer(4)
        }
      //Opcode 8 is equals: if the first parameter is equal to the second parameter,
        // it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 8 => {
          val updateIndex = getIndex(3)
          if (getValAtIndex(1) == getValAtIndex(2)) {
            updateState(updateIndex, 1)
            println("Setting location "+updateIndex+" to "+1)
          }
          else {
            updateState(updateIndex, 0)
            println("Setting location "+updateIndex+" to "+0)
          }
          incrementPointer(4)
        }
      }
    }
  }

  val instructions = new Instructions(programUpdated, 0)

  @scala.annotation.tailrec
  def getInstructions:Unit = {
    println("The opcode is: "+instructions.getOpcode)
    println(instructions.ins.mkString(" "))

    //instructions.printInstructions

    if (instructions.getOpcode.toInt == 9) {
      instructions.printState
    }
    else {
      instructions.updateInstructions(instructions.getOpcode)
      getInstructions
    }
  }

  getInstructions

}
