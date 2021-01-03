package adventofcode.adventofcode


object day23 {


	def runPart1(initStr: String): String = {

		var init = initStr.toList

		var currentPos = 0
		var taken: List[Char] = List()

		for (i <- (1 to 100)) {
		   
		    
		    val currentVal = init(currentPos)
		    
		    taken = List(init((currentPos + 1) % init.length), 
		                     init((currentPos + 2) % init.length), 
		                     init((currentPos + 3) % init.length))
		    
		    val remains = (4 to init.length).map(i => init((currentPos + i) % init.length))
		    val remSorted = remains.sortBy(- _.toInt)

		    val currentPosSorted = remSorted.indexOf(currentVal)

		    val destPos = (currentPosSorted + 1) % remSorted.length
		    val destVal = remSorted(destPos)
		    val destPos2 = remains.indexOf(destVal)
		    init = remains.take(destPos2 + 1).toList ::: taken ::: remains.drop(destPos2 + 1).toList


		    currentPos = init.zipWithIndex.filter(_._1 == currentVal)(0)._2
		    currentPos = (currentPos + 1) % init.length
		}

		val posOne = init.indexOf('1')
		(1 to init.length - 1).map(i => init((posOne + i) % init.length)).mkString

	}


	def runPart2(initStr: String): Long = {


		val N = 1000000
		val nIter = 10000000

		def findDest(curr: Int, a: Int, b: Int, c: Int) = {
		    val abc = List(a, b, c).toSet
		    var nextMin = (curr - 1 - 1 + N) % N + 1 
		    for (i <- 1 to 4)
		    
		        if (abc.contains(nextMin))
		            nextMin = (nextMin - 1 - 1 + N) % N + 1 
		    
		    nextMin
		}

		val next = new Array[Int](N)


		var init = initStr.toList.map(_.toString.toInt)

		(0 to init.length - 1).foreach(i => {
		  
		    if (i < init.length - 1)
		        next(init(i) - 1) = init(i + 1)
		    else
		        next(init(i) - 1) = (init.length + 1) % N
		})

		(init.length to N - 1).foreach(i => next(i) = i + 2)
		next(N - 1) = init(0)

		var curr = init(0)
		for (i <- 1 to nIter){
		    
		    var a = next(curr - 1) 
		    var b = next(a - 1) 
		    var c = next(b - 1) 
		    var nextStep = next(c - 1)
		    next(curr - 1) = nextStep
		    var dest = findDest(curr, a, b, c)
		    var oldNext = next(dest - 1)
		    next(dest - 1) = a
		    next(c - 1) = oldNext
		    curr = next(curr - 1)
		    
		} 
		next(0).toLong * next(next(0) - 1).toLong
	}

	def main(args: Array[String]): Unit = {

		val init = "193467258"
		println(runPart1(init))
		println(runPart2(init))

	}






}