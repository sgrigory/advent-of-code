package adventofcode.adventofcode

object day2 {

	def testRule(entry: String): Boolean = {
	    var line = entry.split(":")
	    var rule = line(0).split(" ")
	    var bounds = rule(0).split("-").map(_.toInt)
	    var chr = rule(1).charAt(0)
	    var text = line(1).split(" ")(1)
	    var len = text.toList.filter(_ == chr).length
	    return (bounds(0) <= len) && (len <= bounds(1))
	    }

	def testRule2(entry: String): Boolean = {
	    var line = entry.split(":")
	    var rule = line(0).split(" ")
	    var bounds = rule(0).split("-").map(_.toInt)
	    var chr = rule(1).charAt(0)
	    var text = line(1).split(" ")(1)
	    var len = text.toList.length
	    return (len >= bounds(0)) && (len >= bounds(1)) && ((text(bounds(0) - 1) == chr) ^ (text(bounds(1) - 1) == chr))
	    }

	val lines = scala.io.Source.fromFile(path + "input_2020_2.txt").getLines().toList

	def main(args: Array[String]): Unit = {

	  	println(lines.map(testRule).filter(identity).length)
	
		println(lines.map(testRule2).filter(identity).length)
	 
	}

}