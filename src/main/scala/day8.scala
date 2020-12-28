package adventofcode.adventofcode

import util.control.Breaks._

object day8 {


	case class Line(var cmd: String, value: Int, var seen: Boolean)


	def main(args: Array[String]): Unit = {

	val fileName = "input_2020_8.txt"
	val lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	var instr = lines.map(_.split(" ")).map(x => Line(x(0), x(1).toInt, false))

	// Part 1: Infinite loop

	var j = 0
	var acc = 0
	do {
	    instr(j).seen = true
	    (instr(j).cmd) match {
	        case "jmp" => j += instr(j).value.toInt
	        case "acc" => {acc += instr(j).value.toInt; j += 1}
	        case "nop" => j += 1
	        
	    }

	} while (! instr(j).seen)

	println(acc)

	// Part 2: After fixing the instructions

	val swap  = Map("jmp" -> "nop", "nop" -> "jmp")

	for (pos <- (0 to instr.length - 1)) {
	    var instr = lines.map(_.split(" ")).map(x => Line(x(0), x(1).toInt, false))
	    if (instr(pos).cmd != "acc")
	        {
	        instr(pos).cmd = swap(instr(pos).cmd)
	        
	        var j = 0
	        var acc = 0
	        do {
	            //println(instr(j), acc)
	            instr(j).seen = true
	            (instr(j).cmd) match {
	                case "jmp" => j += instr(j).value.toInt
	                case "acc" => {acc += instr(j).value.toInt; j += 1}
	                case "nop" => j += 1

	            }

	        } while ((j < instr.length) && (!instr(j).seen))
	            
	        if (j >= instr.length)
	            {
	                println("found")
	                println(acc)
	                break
	            }
	    }
	}

}

}