@main def Main(): Unit = {
    def get1(): Int = {
        scala.io.StdIn.readLine.toInt
    }
    def get2(): (Int, Int) = {
        val s = scala.io.StdIn.readLine.split("\\s+")
        s(0).toInt -> s(1).toInt
    }

    val total = get1()
    val t = get1()
    var count = 0
    for _ <- Range(0,t) do {
        val (a,b) = get2()
        count += (a * b)
    }
    println(if count == total then "Yes" else "No")
}