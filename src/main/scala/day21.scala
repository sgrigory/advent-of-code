package adventofcode.adventofcode


object day21 {

	def main(args: Array[String]): Unit = {

		val fileName = "input_2020_21.txt"
		var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

		val patternLine = """(.+) \(contains (.+)\)""".r

		val allMaps = lines.map(x => x match {
	    case patternLine(a, b) => {
	       b.replace(" ", "").split(",").toList.map(x => x -> a.split(" ").toList)
	        
	    }
	    case _ => {println("!!!")
	    List()}
		})
		
		// Part 1

		val m2 = allMaps.flatten
		val allerg2Ingr = m2.groupBy(_._1).mapValues(_.map(_._2.toSet).reduce((a, b) => a.intersect(b)))
		val ingrCanContain = allerg2Ingr.map(_._2).reduce(_.union(_))
		val allIngr = m2.map(_._2).reduce(_.union(_)).toSet
		val ingCantContain = allIngr.diff(ingrCanContain)
		println(allMaps.map(_(0)._2.toSet.intersect(ingCantContain).size).sum)

		// Part 2
		val m4 = allerg2Ingr.map(x => x._2.map(y => y -> x._1).toList).flatten.groupBy(_._1).mapValues(_.map(_._2))
		var canonicalList: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
		var used: Set[String] = Set()
		m4.toList.sortBy(_._2.size).foreach(x => {
		    val nonUsed: Set[String] = x._2.toSet.diff(used)
		    if (nonUsed.size == 1) {
		        
		        canonicalList(x._1) = nonUsed.toList(0) 
		        used += nonUsed.toList(0)
		    } else
		    {
		        println("!!!")
		    }
		    
		}

		)

		println(canonicalList.toList.sortBy(_._2).map(_._1).mkString(","))

	}



}