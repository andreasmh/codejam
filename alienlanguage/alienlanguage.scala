/* Google Code Jam problem.
* Code Jam 2009 Qualification Round
*
* Problem: Alien Language
*
*/
object AlienLanguage {
	
	def main(args: Array[String]): Unit = {
		import scala.util.matching.Regex
		import scala.io.Source._
		import java.io._

		val lines = fromFile("test.in").getLines
		var vars = lines.next().split(" ")
		/* Word length: L */
		val l = vars(0).toInt
		/* Dictionary/Number of words: D */
		val d = vars(1).toInt
		/* Number of test cases: N */
		val n = vars(2).toInt

		/* Create dictionary from file */
		val dic: Array[String] = (lines take d).toArray

		/* Make each testcase into a regular expression by changing the
		 parentheses to brackets*/
		val cases: Array[Regex] = new Array[Regex](n)		
		for(i <- 0 until n) {
			cases(i) = lines.next().replace('(','[').replace(')',']').r
		}

		/* Compare each dictionary word to the regular expressions and
		create an array in which to save the results */
		val Result: Array[Int] = new Array[Int](n)
		for(i <- 0 until d){
			for(j <- 0 until n) {
				cases(j).findFirstIn(dic(i)) match {
			  		case Some(_) => Result(j) += 1
			  		case None => 0
				}			
			}
		}

		val writer = new PrintWriter(new File("test.out" ))
		for(i <- 1 to n){writer.println("Case #" + i + ": " + Result(i-1))}
		writer.close
	}
}