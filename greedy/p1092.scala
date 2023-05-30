package greedy.p1092

@main def Main(): Unit = {
    import scala.io.StdIn

    case class Crane(weight: Int, count: Int)
    given sortCrane: Ordering[Crane] = Ordering.fromLessThan{(a,b) => 
        if a._2 == b._2 then {
            a._1 < b._1
        } else {
            a._2 < b._2
        }
    }

    val n = StdIn.readInt()
    var cranes = StdIn.readLine().split(' ').map(a => Crane(a.toInt, 0)).sorted
    val m = StdIn.readInt()
    val boxes = StdIn.readLine().split(' ').map(_.toInt).sortBy(a => -a)

    val bk1 = scala.util.control.Breaks()
    val bk2 = scala.util.control.Breaks()
    var done = false

    bk2.breakable{
        for i <- 0 until m do {        
            val curBox = boxes(i)
            var checked = false
            //println(cranes.mkString(" "))
            bk1.breakable {            
                for c <- 0 until n do {
                    val curCrane = cranes(c)
                    if curCrane.weight >= curBox then {
                        cranes(c) = curCrane.copy(count = curCrane.count+1)
                        cranes = cranes.sorted
                        checked = true
                        bk1.break()
                    }
                }
            }     
            if !checked then {
                done = true
                println("-1")    
                bk2.break()        
            }
        }
    }

    val res = cranes.map(a => a.count).max
    if !done then println(res)
}
