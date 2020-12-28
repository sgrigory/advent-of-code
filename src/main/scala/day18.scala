package adventofcode.adventofcode


object day18 {


	val fileName = "input_2020_18.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList


	val patternNum = """([0-9]+).*""".r
	val patternParenth = """\(.+""".r


	val getParenthTerm = (s: String) =>
		{
		    
		   val len = s.map(_ match {
		            case '(' => 1
		            case ')' => -1
		            case _ => 0
		        } ).scanLeft(0)(_ + _).drop(1).takeWhile(_ != 0).length
		    
		   s.take(len + 1)
		}

	val getNextTerm = (s: String, pos: Int, compute: (String => Long)) => {
	    
	    var retVal: Long = 0
	    var retPos = 0
	    s.drop(pos) match {
	        case patternNum(value) => {
	            
	            retVal = value.toLong
	            retPos = pos + value.length
	        }
	        case patternParenth() => {
	            val parenthTerm = getParenthTerm(s.drop(pos))
	            
	            retVal = compute(parenthTerm.drop(1).dropRight(1))
	            retPos = pos + parenthTerm.length
	    }
	}
	  (retPos, retVal)  
	}


	val compute: (String => Long) = (s: String) =>
	{
	
	var pos = 0
	var value: Long = 0
	var op = '+'
	while (pos < s.length){
	    
	    val (pos1, term) = getNextTerm(s, pos, compute)
	   
	    if (op == '+') {
	        
	        value += term
	    }
	    else if (op == '*')
	    {
	        
	        value *= term
	    }
	    op = if (pos1 < s.length) s(pos1) else 'a'
	    pos = pos1 + 1
	    
	}
	
	value
	}

	case class Term(val op: Char, val term: Long)

	val foldFun : (Tuple3[Long, Long, Char], Term) => Tuple3[Long, Long, Char] = (a, b) => {
	    
	    b.op match {
	        case '+' => (a._1, a._2 + b.term, '+')
	        case '*' => (a._1 * a._2, b.term, '*')
	        case _ => (a._1 * a._2, 0, ' ')
	        
	    }
	    
	}

	val compute2: (String => Long) = (s: String) =>
	{
	
	var pos = 0
	var value: Long = 0
	var op = '+'
	var termList: List[Term] = List()
	while (pos < s.length){
	    
	    val (pos1, term) = getNextTerm(s, pos, compute2)
	    termList = termList :+ Term(op, term)
	    op = if (pos1 < s.length) s(pos1) else 'a'
	    pos = pos1 + 1
	    
	}
	
	val startingPoint: (Long, Long, Char) = (1, 0, '+')
	val folded = termList.foldLeft(startingPoint)(foldFun)
	    
	folded._1 * folded._2
	}



	


	def main(args: Array[String]): Unit = {

		println(lines.map(x => compute(x.replace(" ", "")).toLong).sum)
		
		println(lines.map(x => compute2(x.replace(" ", "")).toLong).sum)
	}
}

