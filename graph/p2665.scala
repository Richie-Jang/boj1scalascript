object p2665 {

    import java.io.BufferedReader
    import java.io.InputStreamReader
    import scala.math.Ordering

    var n: Int = 0
    var grid: Array[Array[Int]] = _
    var visits: Array[Array[Int]] = _

    class Info(val x: Int, val y: Int, val value: Int) {
        override def toString(): String = s"Info[x:$x,y:$y,value:$value]"    
    }

    val dirs = Seq(
        -1 -> 0, 
        1 -> 0,
        0 -> -1,
        0 -> 1    
    )

    def getNexts(x: Int, y: Int): Seq[(Int,Int)] = 
        dirs.map {case (a,b) => (a+x, b+y)}.filter {case (a,b) => a >= 0 && a < n && b >= 0 && b < n}

    def bfs(): Unit = {
        given Ordering[Info] = Ordering.by(-1 * _.value)
        val q = scala.collection.mutable.PriorityQueue.empty[Info]
        
        q.enqueue(Info(0,0,0))    
        visits(0)(0) = 0

        import scala.util.control.Breaks.*
        
        breakable {
            while q.nonEmpty do {
                val cur = q.dequeue()        
                val curValue = visits(cur.y)(cur.x)            
                if cur.x == n-1 && cur.y == n-1 then break()   

                getNexts(cur.x, cur.y).foreach{ case (a,b) =>                 
                    val g = grid(b)(a)
                    val nextValue = curValue + (if g == 1 then 0 else 1)
                    if visits(b)(a) > nextValue then {
                        visits(b)(a) = nextValue                    
                        q.enqueue(Info(a,b,nextValue))
                    }
                }        
            }
        }

        println(visits(n-1)(n-1))
        
    }

    def main(args: Array[String]): Unit = {
        val br = new BufferedReader(new InputStreamReader(System.in)) 
        n = br.readLine().toInt
        grid = Array.ofDim[Array[Int]](n)
        visits = Array.fill(n,n)(Int.MaxValue)
        for y <- Range(0, n) do {
            grid(y) = br.readLine().map(Character.digit(_,10)).toArray
        }
        bfs()
        br.close()
}
}