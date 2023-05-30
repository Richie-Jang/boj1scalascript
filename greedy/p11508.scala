package greedy.p11508

@main def Main(): Unit = {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val br = new BufferedReader(new InputStreamReader(System.in))
    val n = br.readLine().toInt

    var arr = new Array[Int](n)
    for i <- 0 until n do {
        arr(i) = br.readLine().toInt
    }
    arr = arr.sortBy(a => -1 * a)

    if n == 1 || n == 2 then {
        println(arr.sum)
        return
    }

    var res = 0L
    for i <- 0 until n do {
        if i % 3 != 2 then {
            res += arr(i).toLong
        }
    }
    println(res)
    br.close()
}