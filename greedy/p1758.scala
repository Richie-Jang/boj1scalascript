package greedy.p1758

@main def Main(): Unit = {

    import java.io.{BufferedReader, InputStreamReader}

    val br = new BufferedReader(new InputStreamReader(System.in))
    val n = br.readLine().toInt

    var rank = 1
    var sum = 0L
    
    var arr = new Array[Int](n)

    for i <- 0 until n do {        
        arr(i) = br.readLine().toInt
    }

    br.close()

    arr = arr.sortBy(a => -1 * a)

    def loop(cur: Int): Unit = {
        if cur >= n then return
        val tip = arr(cur) - (rank - 1)
        if tip <= 0 then return
        sum += tip.toLong
        rank += 1
        loop(cur+1)
    }

    loop(0)

    println(sum)

}