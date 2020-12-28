package adventofcode.adventofcode

object day5 {

	def main(args: Array[String]): Unit = {

		val fileName = "input_2020_5.txt"
		val lines = scala.io.Source.fromFile(path + fileName).getLines()
		val ids = lines.map(s => Integer.parseInt(s.replaceAll("B|R", "1").replaceAll("F|L", "0"), 2))
		println(ids.toList.sorted.sliding(2).takeWhile(i => i(0) + 1 == i(1)).toList.last(1) + 1)

	}

}