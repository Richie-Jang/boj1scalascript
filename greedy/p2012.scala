package greedy.p2012

@main def Main(): Unit = {

    import java.io.BufferedReader
    import java.io.InputStreamReader

    val br = new BufferedReader(new InputStreamReader(System.in))
    val n = br.readLine().toInt

    var arr = new Array[Int](n)
    for i <- 0 until n do {
        arr(i) = br.readLine().toInt        
    }
    arr = arr.sorted
    br.close()

    var sum = 0L
    for i <- 0 until n do {
        val cur = arr(i)
        val rank = i+1
        val m = scala.math.abs(cur - rank)
        sum += m.toLong
    }

    println(sum)
}