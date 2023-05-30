package greedy.p1052

@main def Main(): Unit = {
    import scala.io.StdIn

    val (n,k) = {
        val r = StdIn.readLine().split(' ').map(_.toInt)
        r(0) -> r(1)
    }

    def countBottles(v: Int): Int = {
        var bottles = v
        var count = 0
        while bottles > 0 do {
            val mergedBottles = bottles / 2
            val remained = bottles % 2
            if remained != 0 then {
                count += 1
            }
            bottles = mergedBottles
        }
        count
    }

    def loop(v: Int, count: Int): Int = {
        if v <= k then {            
            return count
        }
        
        val bt = countBottles(v)
        if bt <= k then {
            return count
        }
        loop(v+1, count+1)
    }

    val res = loop(n, 0)
    println(res)
}