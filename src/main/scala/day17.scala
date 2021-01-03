package adventofcode.adventofcode


object day17 {

	val fileName = "input_2020_17.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	trait Cubes[Point] {
    
    type Cells = Set[Point]
    def Cells(xs: Point*) = Set[Point](xs: _*)

    def shifts: Cells
    def applyShift(cell: Point, shift: Point): Point
    def unproject(x: Int, y: Int): Point
    
    def getNeighbours(cell: Point): Cells = {
    
       shifts.map(s => applyShift(cell, s)).toSet
    } 
    
    def countNbs(cell: Point, activeCells: Cells): Int = {
    
        val neightbours: Cells = getNeighbours(cell)
        neightbours.intersect(activeCells).size

    }
    
    def iterate(nIterations: Int): Int = {
        
        var activeCells = Cells()

        lines.zipWithIndex.foreach(line => (0 to line._1.length - 1).foreach(i => if (line._1(i) == '#')
                                                                                 activeCells += (unproject(line._2, i))
                                                                        ))

        
        for (iteration <- 1 to nIterations){

            var newActive = Cells()

            val neighbours: Cells = activeCells.flatMap(c => getNeighbours(c))
            val inactiveCells = neighbours.diff(activeCells)


            for (cell <- activeCells)
                if ((countNbs(cell, activeCells) == 2) | (countNbs(cell, activeCells) == 3))
                    newActive += cell

            for (cell <- inactiveCells)
                if (countNbs(cell, activeCells) == 3)
                    newActive += cell

            activeCells = newActive
            println(iteration, activeCells.size)
        }
          activeCells.size  
        }
    
}
               
type Point4D = (Int, Int, Int, Int)
type Point3D = (Int, Int, Int)
val shs = List(-1, 0, 1)

class Cubes3D extends Cubes[Point3D] {
    
    def applyShift(cell: Point3D, shift: Point3D): Point3D = {
    
    (cell._1 + shift._1, cell._2 + shift._2, cell._3 + shift._3)
}
    
    def shifts = shs.flatMap(x => shs.flatMap(z => shs.map(y => (x, y, z)))).filter(_ != (0, 0, 0)).toSet

    def unproject(x: Int, y: Int): Point3D = (x, y, 0)
}

class Cubes4D extends Cubes[Point4D] {
    
    def applyShift(cell: Point4D, shift: Point4D): Point4D = {
    
    (cell._1 + shift._1, cell._2 + shift._2, cell._3 + shift._3, cell._4 + shift._4)
}
    
    def shifts = shs.flatMap(x => shs.flatMap(y => shs.flatMap(z => shs.map(w => (x, y, z, w))))).filter(_ != (0, 0, 0, 0)).toSet
    def unproject(x: Int, y: Int): Point4D = (x, y, 0, 0)
}


def main(args: Array[String]): Unit = {


	println(new Cubes3D().iterate(6))
	println(new Cubes4D().iterate(6))

}



}