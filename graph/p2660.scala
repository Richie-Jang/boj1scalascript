object p2660 {
    import java.io.{BufferedReader, InputStreamReader}    
    import scala.util.control.Breaks
    import scala.collection.mutable

    var n: Int = _
    val adjs: mutable.Map[Int, Vector[Int]] = mutable.Map.empty

    def main(args: Array[String]): Unit = {

        val br = new BufferedReader(new InputStreamReader(System.in))
        n = br.readLine().toInt
        for i <- 1 to n do {
            adjs.update(i, Vector.empty)
        }

        val brk1 = Breaks()
        brk1.breakable {
            while true do {
                val inp = br.readLine().split("\\s+").map(_.toInt)
                val a = inp(0)
                val b = inp(1)
                if a == -1 && b == -1 then brk1.break()
                adjs.update(a, adjs(a) :+ b)
                adjs.update(b, adjs(b) :+ a)
            }
        }

        br.close()

        // for bfs
        val rankMap = mutable.Map.empty[Int, Int]

        def bfs(cur: Int): Unit = {
            val q = mutable.Queue.empty[(Int,Int)]
            val visits = mutable.Set.empty[Int]
            q.enqueue(cur -> 0)
            visits += cur
            var maxRank = -1
            while q.nonEmpty do {
                val pos = q.dequeue()
                if pos._2 > maxRank then maxRank = pos._2
                val nxs = adjs(pos._1).filter{ a => !visits(a) }
                nxs.foreach{ a => 
                    q.enqueue(a -> (pos._2+1))
                    visits += a
                }
            }
            rankMap.update(cur, maxRank)
        }


        for i <- 1 to n do {
            bfs(i)            
        }

        var minValue = Int.MaxValue
        val minCountMap = mutable.Map.empty[Int,Vector[Int]]
        for (k,v) <- rankMap do {
            if v < minValue then { 
                minValue = v
                minCountMap.update(v, minCountMap.getOrElse(v, Vector.empty) :+ k)
            } else {
                if minCountMap.contains(v) then {
                    minCountMap.update(v, minCountMap(v) :+ k)
                } else {
                    minCountMap.update(v, Vector(k))
                }
            }
        }

        val minCount = minCountMap(minValue).size
        val items = minCountMap(minValue).sorted
        println(s"$minValue $minCount")
        println(s"${items.mkString(" ")}")
    }

}