package adventofcode.adventofcode

object day6 {

	def main(args: Array[String]): Unit = {

		val fileName = "input_2020_6.txt"
		val lines = scala.io.Source.fromFile(path + fileName).getLines().toList

		println(lines.mkString(";").split(";;").map(s => s.replace(";", "").toSet.size).reduce(_ + _))

		val sets = lines.mkString(";").split(";;").map(s => s.split(";").map(x => x.toSet).reduce((x, y) => x.intersect(y) ))
		println(sets.map(_.size).sum)
	}

}