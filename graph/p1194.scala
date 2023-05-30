object p1194 {

    import java.io.{BufferedReader, InputStreamReader}
    
    var (n,m) = 0 -> 0
    var grid: Array[Array[Char]] = _
    var (spx,spy) = -1 -> -1
    val next = Array(
        -1 -> 0,
        1 -> 0,
        0 -> -1,
        0 -> 1
    )

    class Info(val x: Int, val y: Int, val count: Int, val keyValue: Int) {
        override def toString(): String = s"[Info]x:$x, y:$y, count:$count, keyValue:$keyValue"
    }

    def getInts(br: BufferedReader): Unit = {
        val s = br.readLine().split("\\s+").map(_.toInt)
        n = s(0)
        m = s(1)
    }

    def bsf(): Unit = {
        import scala.util.control.Breaks
        import scala.collection.mutable

        val startPos = Info(spx, spy, 0, 0)    
        val q = mutable.Queue.empty[Info]
        // (x,y,key)
        val visits = mutable.HashSet.empty[(Int,Int,Int)]

        val keyRooms = Set('a','b','c','d','e','f')
        val roomForKeys = Set('A','B','C','D','E','F')
        
        q.enqueue(startPos)
        visits += ((startPos.x, startPos.y, startPos.keyValue))

        // newkey
        def makeNewKey(c: Char, prevKeyValue: Int): Int = {
            val cc = 1 << (c - 'a')
            prevKeyValue | cc        
        }

        val hasKeyForThisRoom = {(room: Char, key: Int) => Boolean 
            val cc = 1 << (room - 'A')
            (key & cc) == cc
        }

        val bk1 = Breaks()    
        val bk2 = Breaks()
        var res = -1

        def printQ(): Unit = {
            q.foreach(a => println(s"    $a"))
        }

        bk1.breakable {
            while q.nonEmpty do {
                val cur = q.dequeue()        
                if grid(cur.y)(cur.x) == '1' then {
                    res = cur.count
                    bk1.break()
                }
                val keyValue = cur.keyValue
                //println(cur)
                //printQ()
                for (ny,nx) <- next do {
                    bk2.breakable {
                        val y = ny + cur.y
                        val x = nx + cur.x
                        if y < 0 || y >= n || x < 0 || x >= m then bk2.break()      
                        val visitKey = (x,y,keyValue)              
                        if visits.contains(visitKey) then bk2.break()
                        grid(y)(x) match {
                            case '#' => ()
                            case '.' | '0' | '1' => 
                                q.enqueue(Info(x,y,cur.count+1,keyValue))
                                visits += visitKey
                            case aa if keyRooms.contains(aa) => 
                                val nkey = makeNewKey(aa, keyValue)
                                q.enqueue(Info(x,y,cur.count+1, nkey))
                                visits += ((x,y,nkey))
                            case aa if roomForKeys.contains(aa) && hasKeyForThisRoom(aa, keyValue) =>                         
                                q.enqueue(Info(x,y,cur.count+1,keyValue))
                                visits += visitKey
                            case _ => ()
                        }
                    }
                }
            }
        }

        println(res)
    }

    def main(args: Array[String]): Unit = {

        val br = new BufferedReader(new InputStreamReader(System.in))
        getInts(br)
        grid = Array.ofDim(n)    
        for y <- Range(0, n) do {
            grid(y) = br.readLine().toArray
            val idx = grid(y).indexWhere(c => c == '0')
            if idx > -1 then {
                spx = idx
                spy = y            
            }
        }
        br.close()
        bsf()

    }
}