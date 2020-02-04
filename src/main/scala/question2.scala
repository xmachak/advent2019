import scala.io.Source

object question2 extends App {
  def toInt(in: String): Int = in.toInt
  val input = Source.fromFile("/Users/christina/hub/advent2019/input2.txt").getLines.mkString.split(",").map(_.toInt)
  //before running the program, replace position 1 with the value 12 and replace position 2 with the value 2

  def op1(pos1: Int, pos2: Int, pos3: Int, oldArr: Array[Int]): Array[Int] = {
    val newVal = oldArr(pos1) + oldArr(pos2)
    oldArr.updated(pos3, newVal)
  }

  def op2(pos1: Int, pos2: Int, pos3: Int, oldArr: Array[Int]): Array[Int] = {
    val newVal = oldArr(pos1)*oldArr(pos2)
    oldArr.updated(pos3, newVal)
  }

  val nouns = (0 to 99).toArray
  val verbs = (0 to 99).toArray

  val combos = for (noun <- nouns; verb <- verbs) yield (noun,verb)

    for((i,j) <- combos) {
      val program = input.updated(1, i).updated(2, j)
      val programChunks = program.sliding(4,4).toArray

      var currentProgram = program.clone()

      for (chunk <- programChunks){
        if(chunk(0) == 99) {
          if (currentProgram(0) == 19690720) {
            println("Program finished!")
            print(100*i+j)
          }
        }

        if(chunk(0) == 1){
          currentProgram = op1(chunk(1), chunk(2), chunk(3), currentProgram)
        }

        if(chunk(0) == 2){
          currentProgram = op2(chunk(1), chunk(2), chunk(3), currentProgram)
        }
      }
    }
}
