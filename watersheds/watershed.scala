/* Google Code Jam problem.
* Code Jam 2009 Qualification Round
*
* Problem: Watersheds
*
*/
import scala.collection.mutable.MutableList 


object Watershed {
	
	def main(args: Array[String]): Unit = {
		import scala.io.Source._
		import java.io._
		val writer = new PrintWriter(new File("large.out" ))
		val chars: Array[Char] = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
		val maps: MutableList[(Array[Int], Int, Int)] = MutableList()
		val results: MutableList[Array[Char]] = MutableList()
		val lines = fromFile("large.in").getLines
		/* Number of maps: T*/
		var t = lines.next().toInt
		for(i <- 0 until t){
			var dims = lines.next().split(" ")
			var height = dims(0).toInt
			var width = dims(1).toInt
			var res = solveShed(createMap(height, width), height, width)
			writer.println("Case #"+(i+1)+":")
			for(row <- 0 until height){
				for(col <- 0 until width)
					writer.print(chars(res(col + row*width)-1)+" ")
				writer.println()
			}
		}

		def createMap(height: Int, width: Int): Array[Int] = {
			var elements = new Array[Int](height*width)
			for(i <- 0 until height) {
				var cells = lines.next().split(" ")
				for(j <- 0 until width) {
					elements(j + i*width) = cells(j).toInt
				}
			}
			return elements
		}
		
		def solveShed(elements: Array[Int], height: Int, width: Int): Array[Int] = {
			var current = 0
			val results = new Array[Int](height*width)
			for(row <- 0 until height)
				for(col <- 0 until width){
					if(results(col + row*width) == 0){
						getBasin(col, row)
					}
				}

			def getBasin(x:Int, y: Int): Int = {
				val index = (x+y*width)
				val directions = List((x,y,'sink), (x,y-1,'north), (x-1,y, 'west), (x+1,y, 'east), (x,y+1, 'south))
				if(results(x + y*width) == 0){
					getFlow(directions, None) match {
						case 'north => results(index) = getBasin(x,y-1)
						case 'west  => results(index) = getBasin(x-1,y)
						case 'east  => results(index) = getBasin(x+1,y)
						case 'south => results(index) = getBasin(x,y+1)
						case 'sink  => {
							current += 1
							results(index) = current
						}
					}
				}
				return results(index)
			}
	
			def getFlow(list: List[(Int, Int, Symbol)],current: Option[(Int, Int, Symbol)]): Symbol = {
				def checkFlow(x:Int,y:Int,a:Int,b:Int) = {elements(x + y*width) < elements(a + b*width)}
				(list, current) match {
					case (head::tail, None) => getFlow(tail, Some(head))
					case (Nil, Some(a)) => a._3
					case (head::tail, Some(b)) => head match {
						case(x,y,'north) if(y >= 0 && checkFlow(x,y,b._1,b._2)) => getFlow(tail, Some(head))
						case(x,y,'west) if (x >= 0 && checkFlow(x,y,b._1,b._2)) => getFlow(tail, Some(head))
						case(x,y,'east) if(x < width && checkFlow(x,y,b._1,b._2)) => getFlow(tail, Some(head))
						case(x,y,'south) if(y < height && checkFlow(x,y,b._1,b._2)) => getFlow(tail, Some(head))
						case _ => getFlow(tail, Some(b))
					}
				}
			}

			
			return results
		}
		writer.close
	}
}

