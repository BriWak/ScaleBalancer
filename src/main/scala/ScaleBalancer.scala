class BalanceNotPossibleException extends Exception

object ScaleBalancer {

  /*
  * A function that will return a String of one or two numbers which
  * represent the values needed to balance a pair of weights on a scale.
  *
  * input format: "[1,2], [4,5,6]"
  *  - [1,2] represents weights to be balanced
  *  - [4,5,6] represents the weights available to achieve this balance
  * */

  def balance(input: String): String = {

    val splitString = input.replaceAll("\\[","").replaceAll("\\]","").split(",").map(_.toInt)
    val lScale = splitString(0)
    val rScale = splitString(1)
    val weightList = splitString.slice(2,splitString.length)
    val diff = math.abs(lScale - rScale)
    val foundMatch = weightList.map(x => x + lScale) intersect weightList.map(x => x + rScale)

    if (diff == 0) ""
    else if (weightList.contains(math.abs(lScale - rScale))){
      diff.toString}
    else if (foundMatch.length > 0){
      (foundMatch(0) - lScale).toString + "," + (foundMatch(0) - rScale).toString
    } else throw new BalanceNotPossibleException
  }

}

