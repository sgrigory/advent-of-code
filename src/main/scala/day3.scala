package adventofcode.adventofcode

object day3 {

	val lines = scala.io.Source.fromFile(path + "input_2020_3.txt").getLines().toList

	def numTrees(stepX: Int, stepY: Int): BigInt = {
	    return lines.foldLeft((0, 0))((counter, line) => 
	                                           (counter._1 + 1, 
	                                            counter._2 + (if ((counter._1 % stepY == 0) && (line(stepX * counter._1 / stepY % line.length) == '#'))
	                                                          1 
	                                                          else 
	                                                          0
	                                                         )))._2
	}


	def main(args: Array[String]): Unit = {
		

		println(lines.toStream.foldLeft((0, ""))((counter, 
												  line) => (counter._1 + 1, 
															counter._2 + line(3 * counter._1 % line.length)))._2.filter(_ == '#').length)

	}

}