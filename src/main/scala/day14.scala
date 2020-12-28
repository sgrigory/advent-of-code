package adventofcode.adventofcode


object day14 {


	val fileName = "input_2020_14.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	val patternMask = """mask = ([X01]+)""".r
	val patternMem = """mem\[(\d+)\] = (\d+)""".r


	val toIntWMask = (x: Long, lenMask: Int) =>
		String.format(s"%$lenMask" + "s", x.toBinaryString).replace(" ", "0")

	val expandToMask = (x: Long, maskBits: Vector[Int]) => {
		    (toIntWMask(x, maskBits.length) zip maskBits).map(y => y._1.toLong << y._2).sum
		}


	val applyValue = (value: Int, mask: String) =>  {
	    
	    val ones = BigInt(mask.replace("X", "0"), 2)
	    val zeros = BigInt(mask.replace("X", "1"), 2)

	    (value & zeros) | ones
	}


	val applyValue2 = (address: Long, mask: String) =>  {
	    val ones = BigInt(mask.replace("X", "0"), 2).toLong
	    val zeros = BigInt(mask.replace("X", "1"), 2).toLong
	    val maskXs = BigInt(mask.replace("1", "0").replace("X", "1"), 2).toLong
	    val maskBits = mask.zipWithIndex.filter(_._1 == 'X').map(mask.length - _._2 - 1).toVector
	    val addressOnes = address | ones
	    val n = 1 << maskBits.length
	    val ret1 = (0 to n - 1).map(x => expandToMask(x, maskBits))
	    val ret  = ret1.map(x => ~(~(addressOnes | (x & maskXs)) | ((~x) & maskXs)))
	    ret
	}

	def main(args: Array[String]): Unit = {
		var mask = ""
		var values: scala.collection.mutable.Map[Int, BigInt] = scala.collection.mutable.Map()
		lines.foreach(line => {
		    line match  {
		    case patternMask(maskExpr) => mask = maskExpr
		    case patternMem(addr, value) => values(addr.toInt) = applyValue(value.toInt, mask)
		    case _ => println("not found")
		}
		    
		})

		println(values.values.sum)


		

		mask = ""
		var values1: scala.collection.mutable.Map[Long, Long] = scala.collection.mutable.Map()
		lines.foreach(line => {
		    line match  {
		    case patternMask(maskExpr) => mask = maskExpr
		    case patternMem(addr, value) => {
		        
		        var addresses = applyValue2(addr.toLong, mask)
		        addresses.foreach(address => values1(address) = value.toLong)
		    }
		    case _ => println("not found")
		}
		    
		})

		println(values1.values.sum)


}


}