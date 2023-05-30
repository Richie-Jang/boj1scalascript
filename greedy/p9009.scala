package greedy.p9009

@main def Main(): Unit = {

    val map = scala.collection.mutable.TreeMap.empty[Int, Int]
    def makeFib(a: Int): Int = {
        if a == 0 then {
            map.getOrElseUpdate(a, 0)
        } else if a == 1 then {
            map.getOrElseUpdate(a, 1)
        } else {
            val a1 = if map.contains(a-1) then map(a-1) else {
                makeFib(a-1)
            }
            val a2 = if map.contains(a-2) then map(a-2) else makeFib(a-2)
            val a3 = a1 + a2
            //if a3 > 1_000_000_000 then println(a)
            map.put(a, a3)
            a3
        }
    }

    def solveProblem(nv: Int, sb: scala.collection.mutable.StringBuilder): Unit = {
        val bk1 = scala.util.control.Breaks()        
        var res = List.empty[Int]           
        var index = 0
        val ks = map.keys.toArray.sortBy(a => -1 * a)
        var n = nv
        val size = ks.size
        bk1.breakable { 
            while n >= 1 do {
                if n == 1 then {
                    res = 1 :: res
                    bk1.break()
                }
                while index < size && map(ks(index)) > n do {
                    index += 1            
                }
                res = map(ks(index)) :: res
                n -= map(ks(index))
            }
        }

        sb.append(res.mkString(" "))
        sb.append(System.lineSeparator())        
    }

    makeFib(45)
    val br = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    var t = br.readLine().toInt
    val sb = new scala.collection.mutable.StringBuilder()
    while t > 0 do {
        val n = br.readLine().toInt
        solveProblem(n, sb)
        t -= 1
    }
    br.close()
    print(sb.toString)

}