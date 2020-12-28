package adventofcode.adventofcode



object day11 {

	val shifts = List(-1, 0, 1).flatMap(x => List(-1, 0, 1).map(y => (x, y))).filter(_ != (0, 0))

	val numOccup = (lns: List[List[Char]], i: Int, j: Int) => {
    
    	shifts.filter(a => lns(i + a._1)(j + a._2) == '#').length
	} 


	val numVisOccup = (lns: List[List[Char]], i: Int, j: Int) => {
	    
	    shifts.map( shift => {
	        var posI = i + shift._1
	        var posJ = j + shift._2
	        var foundOcc = false
	        while ((!foundOcc) && (posI >= 0) && (posI < lns.length) && (posJ >= 0) && (posJ < lns(posI).length) && (lns(posI)(posJ) != 'L'))
	        {
	            foundOcc ||= lns(posI)(posJ) == '#'
	            posI += shift._1
	            posJ += shift._2
	        
	        } 
	        foundOcc
	    }
	    
	    ).filter(identity).length
	} 


	def main(args: Array[String]): Unit = {

		val fileName = "input_2020_11.txt"
		var lines = scala.io.Source.fromFile(path + fileName).getLines().toList.map("." + _ + ".").map(_.toList)
		lines = List(lines(0).map(x => '.')) ::: lines ::: List(lines(0).map(x => '.'))

		// Part 1
		var change = true

		while (change) {
		    
		    change = false
		    var tmpLines = lines
		    for (i <- 0 to lines.length - 1)
		        for (j <- 0 to lines(i).length - 1)
		            {   if ((lines(i)(j) == 'L') && (numOccup(lines, i, j) == 0))
		                {
		                   
		                    tmpLines = tmpLines.updated(i, tmpLines(i).updated(j, '#'))
		                    change = true
		                }
		            else if ((lines(i)(j) == '#') && (numOccup(lines, i, j) >= 4))
		                {
		                    tmpLines = tmpLines.updated(i, tmpLines(i).updated(j, 'L'))
		                    change = true

		                }
		            }
		    lines = tmpLines
		}
		println(lines.map(x => x.filter(y => y == '#').length).sum)

		// Part 2

		change = true

		while (change ) {
		    
		    change = false
		    var tmpLines = lines
		    for (i <- 0 to lines.length - 1)
		        for (j <- 0 to lines(i).length - 1)
		            {   if ((lines(i)(j) == 'L') && (numVisOccup(lines, i, j) == 0))
		                {
		                   
		                    tmpLines = tmpLines.updated(i, tmpLines(i).updated(j, '#'))
		                    change = true
		                }
		            else if ((lines(i)(j) == '#') && (numVisOccup(lines, i, j) >= 5))
		                {
		                    
		                    tmpLines = tmpLines.updated(i, tmpLines(i).updated(j, 'L'))
		                    change = true

		                }
		            }
		    lines = tmpLines
		    
		    
		}
		println(lines.map(x => x.filter(y => y == '#').length).sum)


	}
}