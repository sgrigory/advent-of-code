package adventofcode.adventofcode

object day4 {


val generateTestYear = (min: Int, max:Int) => {
    val pattern = """\d\d\d\d""".r
    val testYear = (s: String) => {
        s match {
            case pattern() => (s.toInt >= min) & (s.toInt <= max) 
            case _ => false
        }
        
    }
    
    testYear
    
}

val generateTestHeight = (minCm: Int, maxCm:Int, minIn: Int, maxIn:Int) => {
   val patternCm = """\d+cm""".r
   val patternIn = """\d+in""".r
   val testHeight = (s: String) => {
       val s1  = s dropRight 2
       s match {
           case patternCm() => (s1.toInt >= minCm) & (s1.toInt <= maxCm) 
           case patternIn() => (s1.toInt >= minIn) & (s1.toInt <= maxIn) 
           case _ => false
       }
       
   }
   
   testHeight
   
}

val generateTestPattern = (pattern: String) => {
   val patternCompiled = pattern.r
   val testPattern = (s: String) => {
       s match {
           case patternCompiled() => true 
           case _ => false
       }
       
   }
   
   testPattern
   
}

def main(args: Array[String]): Unit = {

val tests = Map("byr" -> generateTestYear(1920, 2002),
           "iyr" -> generateTestYear(2010, 2020),
           "eyr" -> generateTestYear(2020, 2030),
           "hgt" -> generateTestHeight(150, 193, 59, 76),
           "hcl" -> generateTestPattern("""#[0-9a-f]{6}"""),
           "ecl" -> generateTestPattern("""amb|blu|brn|gry|grn|hzl|oth"""),
           "pid" -> generateTestPattern("""[0-9]{9}""")
          )

val lines = scala.io.Source.fromFile(path + "input_2020_4.txt").getLines().toList

val y = lines.flatMap(_.split(" ")).map(x => if (x == "") "/" else x).mkString(";").split("/")

val y1 = y.map(x => x.split(";").map(z => if (z == "") ("nothing", "nothing") else (z.split(":")(0), z.split(":")(1))).toMap)

val y2 = y1.map(m => tests.keys.toSet.subsetOf(m.keys.toSet) && tests.keys.map(key => tests(key)(m(key))).reduce(_ & _))

println(y2.filter(identity).length)

}

}