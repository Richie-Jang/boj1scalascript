package geom.p2261

@main def Main(): Unit = {

    import java.io.BufferedReader
    import java.io.InputStreamReader
    import scala.util.control.Breaks

    val bf = new BufferedReader(new InputStreamReader(System.in))
    val n = bf.readLine().toInt

    case class Point(x: Int, y: Int) {
        def dist(anotherPoint: Point): Int = {
            val a = x - anotherPoint.x 
            val b = y - anotherPoint.y
            a*a + b*b
        }
    }

    val getCoord = { (s: String) => 
        val s1 = s.trim.split(' ')
        Point(s1(0).toInt, (s1(1).toInt))
    }

    val points = new Array[Point](n)
    for i <- 0 until n do {
        points(i) = getCoord(bf.readLine())
    }

    bf.close()
    
    // sort by x
    points.sortInPlaceWith{(a,b) => a.x < b.x}    

    // left , right are index
    def closetPair(left: Int, right: Int): Int = {
        val len = right - left + 1
        if len == 2 then {
            return points(left).dist(points(right))
        }
        if len == 3 then {
            val a1 = points(left).dist(points(left+1))
            val a2 = points(left).dist(points(left+2))
            val a3 = points(left+1).dist(points(left+2))
            return List(a1,a2,a3).min
        }

        val mid = (right + left) / 2
        val ldist = closetPair(left, mid)
        val rdist = closetPair(mid+1, right)
        var minDist = scala.math.min(ldist, rdist)

        val midPoints = {
            val midp = points(mid)
            points.slice(left, right+1).filter{ p => 
                val xdist = p.x - midp.x
                xdist * xdist < minDist    
            }
        }.sortBy(_.y)

        val bk = Breaks()

        for i <- 0 until midPoints.length-1 do bk.breakable {
            for j <- i+1 until midPoints.length do {
                val ydist = midPoints(i).y - midPoints(j).y
                if ydist * ydist < minDist then {
                    val newDist = midPoints(i).dist(midPoints(j))
                    if newDist < minDist then minDist = newDist
                } else {
                    bk.break()
                }
            }
        }
        minDist
    }

    println(closetPair(0, points.length-1))
}
