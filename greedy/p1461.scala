package greedy.p1461

@main def Main(): Unit = {
    import scala.io.StdIn  
    import scala.math.abs

    val (n, m) = {
        val r = StdIn.readLine().split(' ').map(_.toInt)
        r(0) -> r(1)
    }

    val arr = StdIn.readLine().split(' ').map(_.toInt)
    var (pos, neg) = arr.partition(a => a >= 0)

    pos = pos.sorted.reverse
    neg = neg.sorted
    
    var steps = 0
    val posArr = pos.sliding(m,m).toList
    for a <- posArr do {
        val v = a.map(abs(_)).max
        steps += (v * 2)
    }
    val negArr = neg.sliding(m,m).toList

    def printArr(a: List[Array[Int]]): Unit = {
        println(a.map(b => b.mkString(" ")).mkString(" < "))
    }

    for a <- negArr do {
        val v = a.map(abs(_)).max
        steps += (v * 2)
    }
    val nHead = if neg.isEmpty then 0 else neg.head
    val pHead = if pos.isEmpty then 0 else pos.head
    val maxV = scala.math.max(-1 * nHead, pHead)
    
    steps -= maxV
    println(steps)
}