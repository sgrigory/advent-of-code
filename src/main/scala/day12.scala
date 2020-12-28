package adventofcode.adventofcode

import scala.math._


object day12 {


	val fileName = "input_2020_12.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	case class State(var x: Double, var y: Double, var dir: Double)
	case class StateWayPoint(var xSh: Double, var ySh: Double, var xWp: Double, var yWp: Double)


	val execute = (state: State, cmd: String) => {
	    val cmdVal = cmd.drop(1).toInt
	    cmd(0) match {
	        
	        case 'N' => state.y += cmdVal
	        case 'S' => state.y -= cmdVal
	        case 'E' => state.x += cmdVal
	        case 'W' => state.x -= cmdVal
	        case 'L' => state.dir += cmdVal
	        case 'R' => state.dir -= cmdVal
	        case 'F' => {
	            state.x += cmdVal * cos(Pi * state.dir / 180)
	            state.y += cmdVal * sin(Pi * state.dir / 180)
	        }
	        
	    }
	    state
	}

	val executeWp = (state: StateWayPoint, cmd: String) => {
	    val cmdVal = cmd.drop(1).toInt
	    cmd(0) match {
	        
	        case 'N' => state.yWp += cmdVal
	        case 'S' => state.yWp -= cmdVal
	        case 'E' => state.xWp += cmdVal
	        case 'W' => state.xWp -= cmdVal
	        case 'L' => {
	                    val xWpNew = state.xWp * cos(Pi * cmdVal / 180) - state.yWp * sin(Pi * cmdVal / 180)
	                    val yWpNew = state.xWp * sin(Pi * cmdVal / 180) + state.yWp * cos(Pi * cmdVal / 180)
	                    state.xWp = xWpNew
	                    state.yWp = yWpNew
	                    }
	        case 'R' => {
	                    val xWpNew = state.xWp * cos(Pi * cmdVal / 180) + state.yWp * sin(Pi * cmdVal / 180)
	                    val yWpNew = - state.xWp * sin(Pi * cmdVal / 180) + state.yWp * cos(Pi * cmdVal / 180)
	                    state.xWp = xWpNew
	                    state.yWp = yWpNew
	                    }
	        case 'F' => {
	            state.xSh += cmdVal * state.xWp
	            state.ySh += cmdVal * state.yWp
	        }
	        
	    }
	    state
	}

def main(args: Array[String]): Unit = {

	val endState = lines.foldLeft(State(0, 0, 0))((state, cmd) => execute(state, cmd))

	println(scala.math.round(scala.math.abs(endState.x) + scala.math.abs(endState.y)))

	val endState2 = lines.foldLeft(StateWayPoint(0, 0, 10, 1))((state, cmd) => executeWp(state, cmd))

	println(scala.math.round(scala.math.abs(endState2.xSh) + scala.math.abs(endState2.ySh)))

	}

}