package greedy.p11501

@main def Main(): Unit = {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val sb = scala.collection.mutable.StringBuilder()
    def solve(n: Int, arr: Array[Int]): Unit = {
        var lastV = arr.last
        var res = 0L
        for i <- n-2 to 0 by -1 do {
            val v = arr(i)
            if v > lastV then {
                lastV = v                
            } else {
                res += (lastV - v).toLong
            }
        }
        sb.append(s"$res\n")
    }
    val br = new BufferedReader(new InputStreamReader(System.in))
    val t = br.readLine().toInt
    for i <- 0 until t do {
        val n = br.readLine().toInt
        val arr = br.readLine().split(' ').map(_.toInt)
        solve(n, arr)
    }
    br.close()
    println(sb.toString())
}