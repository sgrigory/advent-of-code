package adventofcode.adventofcode

object day7 {


	val parseLine = (line: String) => {
		    val pattern = """([a-z]+ [a-z]+) bags contains? (.*)\.""".r
		    val patternContained = """([0-9]+) ([a-z]+ [a-z]+) bags?,?""".r
		    val pattern(container, contained) = line
		    val parsedContained = patternContained.findAllIn(contained).matchData.map(m => m.subgroups).map(x => (x(1), x(0).toInt)).toMap
		    (container, parsedContained)
		}


	def main(args: Array[String]): Unit = {


		// Load and parse the rules
		
		val fileName = "input_2020_7.txt"
		val lines = scala.io.Source.fromFile(path + fileName).getLines().toList
		var rules = lines.map(parseLine).toMap

		// Number of colors which can contain shiny gold

		var rulesTmp = rules
		var newColors = Set("shiny gold")
		var oldSize = 0

		do {
		    oldSize = newColors.size
		    rulesTmp.foreach {
		        
		        keyVal => {
		            if (keyVal._2.keys.toSet.intersect(newColors).size > 0) {
		                
		                newColors += keyVal._1
		                rulesTmp -= keyVal._1
		            }
		            
		        }
		    }
		} while ((oldSize < newColors.size) & (rulesTmp.size > 0))
		println(newColors.size - 1)

		// Total number of bags inside shiny gold

		var totalInside = 0
		var tempBags = List[Tuple2[String, Int]]()
		var newBags = Map("shiny gold" -> 1)
		do {
		    
		   
		    newBags.foreach {
		        tempBags = List()
		        keyVal => {
		            val bagsToAdd = rules(keyVal._1).map(x => (x._1 -> x._2 * keyVal._2)).toList
		            tempBags = tempBags ::: bagsToAdd 
		        }
		        newBags = tempBags.groupBy(_._1).map(x => x._1 -> x._2.map(_._2).sum)
		        
		    }
		    totalInside += tempBags.map(_._2).sum
		} while (newBags.size > 0)
		println(totalInside)


	}



}