package adventofcode.adventofcode

object Part1 {
  def main(args: Array[String]): Unit = {

  	val values = scala.io.Source.fromFile(path + "input_2020_1.txt").getLines().toList.map(_.toInt)

	val pairs = for (i <- 0 to values.length - 1; j <- 0 to i - 1) yield (values(i), values(j))

	val firstPair = pairs.filter(x => x._1 + x._2 == 2020)(0)

	println(firstPair._1 * firstPair._2)
  }
}


object Part2 {
  def main(args: Array[String]): Unit = {

  	val values = scala.io.Source.fromFile(path + "input_2020_1.txt").getLines().toList.map(_.toInt)

	val triplets = for (i <- 0 to values.length - 1; j <- 0 to i - 1; k <- 0 to j - 1) yield (values(i), values(j), values(k))

	val firstTriplet = triplets.filter(x => x._1 + x._2 + x._3 == 2020)(0)

	println(firstTriplet._1 * firstTriplet._2 * firstTriplet._3)
  }
}