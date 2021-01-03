package adventofcode.adventofcode


object day20 {

	val path = "/Users/grisha.oryol/Documents/projects/advent_of_code/"
	val fileName = "input_2020_20.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	val pattern = List(
					"                  # ", 
					"#    ##    ##    ###",
					" #  #  #  #  #  #   "
					)


	def matches(pattern: List[List[Char]], detailed: Array[Array[Char]], i: Int, j: Int): Boolean = {
	    (0 to pattern.length - 1).toList.map(
	        x => (0 to pattern(0).length - 1).toList.map(
	            y => (pattern(x)(y) == ' ') |  (detailed(x + i)(y + j) == '#'))).flatten.reduce(_ & _)
	    
	            
	}

	def applyPattern(pattern: List[List[Char]], detailed: Array[Array[Char]], i: Int, j: Int): Unit = {
	    (0 to pattern.length - 1).toList.map(
	        x => (0 to pattern(0).length - 1).toList.map(
	            y => {if (pattern(x)(y) == '#') 
	                     detailed(i + x)(j + y) = 'O'
	                 }))
	    
	            
	}


	val titlePattern = """Tile ([0-9]+):""".r

	case class Status(var tiles: Map[Int, Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]], var currentTile: List[String])

	def lineToInt(s: String): Int = Integer.parseInt(s.replace("#", "1").replace(".", "0"), 2)

	def createTile(tileLines: List[String]): Tuple8[Int, Int, Int, Int, Int, Int, Int, Int] = {
	    (lineToInt(tileLines(0)), 
	     lineToInt(tileLines(tileLines.length - 1)), 
	     lineToInt(tileLines.map(x => x(0)).mkString), 
	     lineToInt(tileLines.map(x => x(x.length - 1)).mkString),
	     lineToInt(tileLines(0).reverse), 
	     lineToInt(tileLines(tileLines.length - 1).reverse), 
	     lineToInt(tileLines.map(x => x(0)).mkString.reverse), 
	     lineToInt(tileLines.map(x => x(x.length - 1)).mkString.reverse)
	    )
	}


	def cellType(i: Int, j: Int, cells: Array[Array[Int]]): Int = {
	    if ((i > 0) & (i < cells.length - 1) & (j > 0) & (j < cells.length - 1))
	        return 3
	    if (((i == 0) | (i == cells.length - 1)) & ((j == 0) | (j == cells.length - 1)))
	        return 1
	    return 2
	}

	def findFirst(set: Set[Int], usedIDs: Set[Int]): Int = {
	    
	    val available = set.diff(usedIDs).toList
	    if (available.length > 0)
	        {
	            available(0)
	        }
	    else
	        {
	            println("Not found!!!")
	            println(set)
	            println(usedIDs)
	            0
	        }
	}

	def generateMoves[T](image: List[List[T]]): List[List[List[T]]] = {
	    List(image, image.transpose, 
	         image.reverse, image.reverse.transpose, 
	         image.transpose.reverse, image.transpose.reverse.transpose,
	         image.transpose.reverse.transpose.reverse, image.transpose.reverse.transpose.reverse.transpose)
	}

	


	

	def getBorders(i: Int, j: Int, tiles: Array[Array[List[List[Char]]]]): (Int, Int) = {
	    
	    val s1 = if (i == 0) -1 else createTile(tiles(i - 1)(j).map(_.mkString))._2
	    val s2 = if (j == 0) -1 else createTile(tiles(i)(j - 1).map(_.mkString))._4
	    (s1, s2)
	    
	}



def main(args: Array[String]): Unit = {

	val allTiles = lines.filter(_.length > 0).reverse.foldLeft(Status(Map(), List()))((a, b) => b match {
                  case titlePattern(id) => Status(a.tiles + (id.toInt -> createTile(a.currentTile)), List())
                  case _ => Status(a.tiles,  List(b) ::: a.currentTile)
	}).tiles

	val borders = allTiles.map(x => x._2.productIterator.toList.map(y => (x._1, y))).flatten.groupBy(x => x._2).map(x => x._1 -> x._2.map(y => y._1).toSet)

	val allConnections = allTiles.map(x => x._1 -> x._2.productIterator.toList.map(y => borders(y)).reduce((a, b) => a.union(b)))


	val corners = allConnections.map(x => x._1 -> (x._2.size - 1)).filter(_._2 < 3).keys
	val edges = allConnections.map(x => x._1 -> (x._2.size - 1)).filter(_._2 == 3).keys
	val interiors = allConnections.map(x => x._1 -> (x._2.size - 1)).filter(_._2 > 3).keys

	println(corners.map(_.toLong).reduce(_ * _))


	case class StatusContent(var tiles: Map[Int, List[String]], var currentTile: List[String])

	var contentTiles: Map[Int,List[String]] = lines.filter(_.length > 0).reverse.foldLeft(StatusContent(Map(), List()))((a, b) => b match {
	                  case titlePattern(id) => StatusContent(a.tiles + (id.toInt -> a.currentTile), List())
	                  case _ => StatusContent(a.tiles, List(b) ::: a.currentTile)
	}).tiles


	var i = 0
	var j = 0
	val allIDs = allConnections.keys.toSet
	val ln = scala.math.pow(allIDs.size, 0.5).toInt
	var cells1 = Array.ofDim[Int](ln, ln)
	var rotatedTiles = Array.ofDim[List[List[Char]]](ln, ln)
	var usedIDs = Set[Int]()


	def neighbours(i: Int, j: Int, cells: Array[Array[Int]]): Set[Int] = {
	    
	    val s1 = if (i == 0) allIDs else allConnections(cells(i - 1)(j))
	    val s2 = if (j == 0) allIDs else allConnections(cells(i)(j - 1))
	    s1.intersect(s2)
	}


	def findIdOrientation(set: Set[Int], usedIDs: Set[Int], bordersTwo: Tuple2[Int, Int]): (Int, List[List[Char]]) = {
	    
	    val available = set.diff(usedIDs).toList
	    var av: Int = 0
	    var ret = (0, List(List(' ')))
	    if (available.length > 0)
	        {  
	            for (av <- available) {
	                val allRotations = generateMoves(contentTiles(av).map(_.toList))
	                val r1 = allRotations.map(x => x -> createTile(x.map(_.mkString)))
	                val correctRotations = r1.filter(x => (((bordersTwo._1 == -1) | (x._2._1 == bordersTwo._1)) 
	                                                    & ((bordersTwo._2 == -1) | (x._2._3 == bordersTwo._2))) 
	                                                    & ((bordersTwo._1 != -1) | (borders(x._2._2).size > 1))
	                                                    & ((bordersTwo._2 != -1) | (borders(x._2._4).size > 1))
	                                                ).map(_._1)
	                
	                if (correctRotations.length > 0)
	                    {
	                    ret = (av, correctRotations(0))
	                    return ret
	                    }
	            
	            }
	            
	        }
	    else
	        {
	            println("Not found!!!")
	            println(set)
	            println(usedIDs)
	            
	        }
	    
	    ret
	}



	try 
	for (i <- 0 to cells1.length - 1)
	    for (j <- 0 to cells1.length - 1)
	        {   
	            
	            val nbs = neighbours(i, j, cells1)
	            val bordersTwo = getBorders(i, j, rotatedTiles)
	            val idTile = cellType(i, j, cells1) match {
	            case 1 => findIdOrientation(corners.toSet.intersect(nbs), usedIDs, bordersTwo)
	            case 2 => findIdOrientation(edges.toSet.intersect(nbs), usedIDs, bordersTwo)
	            case 3 => findIdOrientation(interiors.toSet.intersect(nbs), usedIDs, bordersTwo)
	            }
	         usedIDs += idTile._1
	         cells1(i)(j) = idTile._1
	         rotatedTiles(i)(j) = idTile._2
	         }
	catch {
	    case e: java.util.NoSuchElementException => println(e)
	}



	val tileSize = contentTiles.values.toList(0).length - 2
	var detailed = Array.ofDim[Char](ln * tileSize, ln * tileSize)

	for (i <- 0 to detailed.length - 1)
	    for (j <- 0 to detailed(0).length - 1)
	    {
	        
	        detailed(i)(j) = rotatedTiles(i / tileSize)(j / tileSize)(i % tileSize + 1)(j % tileSize + 1)
	        
	    }

	val patternLst = pattern.map(_.toList)


	var detailed2 = detailed.map(x => x.map(identity))


	for (patternTest <- generateMoves(patternLst)) {
	    var numMatches = 0
	    for (i <- 0 to detailed.size - patternTest.length)
	        for (j <- 0 to detailed(0).size - patternTest(0).length)
	            if (matches(patternTest, detailed, i, j))
	                {
	                applyPattern(patternTest, detailed2, i, j)
	                numMatches += 1
	                }
    
}
	
	println(detailed2.map(_.filter(_ == '#').size).sum)

	}



}