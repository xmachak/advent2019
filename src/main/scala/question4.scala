import scala.collection.mutable.ArrayBuffer

object question4 extends App {
  val inputStart = 234208
  val inputEnd = 765869

  var codeCounter = 0

  for (i <- inputStart to inputEnd) {
    val iArray = i.toString.toCharArray
    val intArray = iArray.map(_.asDigit)
    var decCounter = 0
    var sameCounter = ArrayBuffer[Int]()
    var inRowCounter = ArrayBuffer[Int]()

    for (j <- 1 until intArray.length) {
      // the code can never decrease
      if (intArray(j-1) > intArray(j)){
        decCounter += 1
      }
      if (intArray(j-1) == intArray(j)){
        sameCounter += intArray(j)
        if((j < 5) && (intArray(j) == intArray(j+1))){
          inRowCounter += intArray(j)
        }
      }
    }
    if ((decCounter < 1) & (sameCounter.length > 0) & (sameCounter.distinct.length > inRowCounter.distinct.length)) codeCounter += 1
  }
  println(codeCounter)
}
