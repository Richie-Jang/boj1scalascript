package greedy.p2457

@main def Main(): Unit = {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val br = new BufferedReader(new InputStreamReader(System.in))
    val n = br.readLine().toInt

    var arr = new Array[(Int,Int)](n)

    val convert = {(s: String) => 
        val a = s.split(' ').map(_.toInt)
        a(0)*100 + a(1) -> (a(2) * 100 + a(3))
    }

    for i <- 0 until n do {
        arr(i) = convert(br.readLine())
    }

    br.close()

    def comp(a: (Int,Int), b: (Int, Int)): Boolean = {
        if a._1 == b._1 then a._2 < b._2 else a._1 < b._1
    }
    // sort
    arr = arr.sortWith(comp)

    var res = 0
    var st = 301
    var et = 0

    import scala.util.control.Breaks

    val bk1 = Breaks()
    var idx = -1
    
    while st <= 1130 && idx < n do {
        idx += 1
        var isChecked = false

        bk1.breakable {
            for i <- idx until arr.length do {
                val cur = arr(i)
                if cur._1 > st then bk1.break()
                if cur._2 > et then {
                    isChecked = true
                    et = cur._2
                    idx = i
                }
            }
        }

        if isChecked then {
            st = et
            res += 1
        } else {
            println("0")
            return
        }
        
    }

    println(res)

}