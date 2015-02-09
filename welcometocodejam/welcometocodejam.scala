/* Google Code Jam problem.
* Code Jam 2009 Qualification Round
*
* Problem: Welcome To Code Jam
*
*/
object WelcomeToCodeJam {
	
	def main(args: Array[String]): Unit = {
		import scala.util.matching.Regex
		import scala.io.Source._
		import java.io._

		val writer = new PrintWriter(new File("large.out" ))
		val lines = fromFile("large.in").getLines
		var nr_cases = lines.next.toInt
		for(i <- 0 until nr_cases) {
			var str = lines.next.dropWhile(_!='w') // drop all letters before first w
			var str2 = str.take(str.lastIndexOf('m')+1) // drop all letters after last m
			var str3 = str2.filter(Set('w','e','l','c','o','m',' ','t','d','j','a')) // filter out all letters that are not in "welcome to code jam"
			var result = subsequence(str3.toList, "welcome to code jam".toList)
			writer.println("Case #"+(i+1)+": " + "%04d".format(result%10000))
			println("Case #"+(i+1)+": " + "%04d".format(result%10000))
		}

		def subsequence(x: List[Char], y:List[Char]): Int = (x,y) match {
			case (x, Nil) => 1
			case (Nil, y) => 0
			case (x,y) if(x.length >= y.length) => {
				var sum = 0
				sum += subsequence(x.tail, y)
				if(x.head == y.head)
						sum += subsequence(x.tail,y.tail)
				return sum
			}
			case _ => 0
		}
		writer.close
	}
}


