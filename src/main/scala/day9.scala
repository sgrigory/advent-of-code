package adventofcode.adventofcode

import util.control.Breaks._

object day9 {

	val generateSums = (lst: List[BigInt]) => {
    
	    //println(lst.flatMap(x => lst.map(y => x + y)).toSet)
	    lst.flatMap(x => lst.map(y => x + y)).toSet
		}

	def main(args: Array[String]): Unit = {

		val fileName = "input_2020_9.txt"
		val lines = scala.io.Source.fromFile(path + fileName).getLines().toList

		// Part 1

		val ints = lines.map(BigInt(_))
		val window = 25
		val retLst = ints.sliding(window + 1).map(x => (x.takeRight(1)(0), 
		                                   x.take(window))).find(x => !generateSums(x._2).contains(x._1))
		val ret1 = retLst.toList(0)._1
		println(ret1)

		// Part 2
		val i = 0
		val seqLen = 0

		var sums = Array.ofDim[BigInt](ints.length, ints.length)

		(0 to ints.length - 1).foreach(j => sums(j)(j) = ints(j))


		for (seqLen <- 1 to ints.length - 1){
		    for (i <- 0 to ints.length - seqLen - 1)
		    {
		     sums(i)(i + seqLen) = sums(i)(i + seqLen - 1) + ints(i + seqLen) 
		     if (sums(i)(i + seqLen) == ret1)
		        {
		            val slice = ints.take(i + seqLen + 1).takeRight(seqLen + 1)
		        }
		    }
		    
		}

		println(sums(ints.length - 1)(ints.length - 1))


	}


}