package greedy.p2828

@main def Main(): Unit = {
    import scala.io.StdIn
    val (n,m) = {
        val r = StdIn.readLine().split(' ').map(_.toInt)
        r(0) -> r(1)
    }
    val drops = StdIn.readLine().toInt
    var buket = Range(0, 0+m)

    var steps = 0
    // update steps
    def moveBucket(pos: Int): Unit = {
        val st = buket.start
        val ed = buket.`end`-1
        if pos < st then {
            val move = st - pos
            steps += move
            buket = Range(buket.start-move, buket.`end`-move)
            return
        }
        if ed < pos then {
            val move = pos - ed
            steps += move
            buket = Range(buket.start+move, buket.`end`+move)
        }
    }

    for i <- 0 until drops do {
        val apple = StdIn.readLine().toInt - 1
        // apple pos check
        if !buket.contains(apple) then {
            moveBucket(apple)
        }
    }

    println(steps)
}