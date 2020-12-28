package adventofcode.adventofcode


object day15 {



	def main(args: Array[String]): Unit = {


		val starting: List[Long] = List(0,3,1,6,7,5)
		val mem: scala.collection.mutable.Map[Long, List[Long]] = scala.collection.mutable.Map()
		var lastSpoken: Long = 0
		for (i <- 0 to starting.length - 1){
		    mem(starting(i)) = List(i + 1) 
		    lastSpoken = starting(i)
		}

		println(lastSpoken)

		val N: Long = 30000000L

		for (i <- starting.length.toLong to (N - 1)){
		    
		    var lastSpokenIdx = mem.getOrElse(lastSpoken, List())
		    if (lastSpokenIdx.length == 2) {
		        lastSpoken = lastSpokenIdx(1) - lastSpokenIdx(0)
		        
		    } else
		    {   
		        lastSpoken = 0
		        
		    }
		    
		    val newSpokenIdx = mem.getOrElse(lastSpoken, List())
		    if (newSpokenIdx.length == 2)
		        mem(lastSpoken) = List(newSpokenIdx(1), i + 1)
		    else if (newSpokenIdx.length == 1)
		        mem(lastSpoken) = List(newSpokenIdx(0), i + 1)
		    else 
		        mem(lastSpoken) = List(i + 1)
		    if (i % 1000000 == 0)
		        println(i)
		}
		println(lastSpoken)



	}


}	