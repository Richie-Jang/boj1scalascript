package greedy.p1969

@main def Main(): Unit = {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val br = new BufferedReader(new InputStreamReader(System.in))
    val (n,m) = {
        val r = br.readLine().split(' ').map(_.toInt)
        r(0) -> r(1)
    }

    val arr = new Array[Array[Char]](n)

    for i <- 0 until n do {
        arr(i) = br.readLine().toArray
    }

    br.close()

    def getCommonChar(cs: Vector[Char]): (Char, Int) = {
        var countMap = Map.empty[Char, Int]
        for c <- cs do {
            if countMap.contains(c) 
            then countMap = countMap.updated(c, countMap(c)+1) 
            else countMap = countMap.updated(c, 1) 
        }
        val a1 = {
            val ks = countMap.keys.toList.sortBy(a => -1*a.toInt)
            var maxC = ks.head
            for k <- ks.tail do {
                val m1 = countMap(maxC)
                val m2 = countMap(k)
                if m1 <= m2 then maxC = k
            }
            maxC
        }
        var changeNum = 0
        for c <- cs do {
            if c != a1 then changeNum += 1
        }
        a1 -> changeNum
    }

    def makeColumnVector(index: Int): Vector[Char] = {
        var res = Vector.empty[Char]
        for i <- 0 until n do {
            res = res :+ arr(i)(index)
        }
        res
    }

    // do
    var res = 0
    var resVec = Vector.empty[Char]
    for i <- 0 until m do {
        val rv = makeColumnVector(i)
        val (c, cn) = getCommonChar(rv)
        resVec = resVec :+ c
        res += cn
    }

    println(resVec.mkString(""))
    println(res)
}