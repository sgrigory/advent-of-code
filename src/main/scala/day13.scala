package adventofcode.adventofcode

import scala.math._


object day13 {

def main(args: Array[String]): Unit = {
	val fileName = "input_2020_13.txt"
	var lines = scala.io.Source.fromFile(path + fileName).getLines().toList

	val ts = lines(0).toInt
	val allBuses = lines(1).split(",")
	val busInfo = allBuses.zipWithIndex.filter(_._1 != "x").map(x => (BigInt(x._1), BigInt(x._2)))
	val intervals = busInfo.map(x => x._1)
	val remTimes = intervals.map(x => (x - ts % x, x)).sorted
	println(remTimes(0)._1 * remTimes(0)._2)

	val N = intervals.reduce(_ * _)

	val summ = busInfo.map(x => (x._1 - x._2 % x._1) * (N / x._1) * (N / x._1).modInverse(x._1))
	println(summ.reduce(_ + _) % N)
}

}