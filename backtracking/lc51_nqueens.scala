package backtracking.lc51

// leetcode 51

import scala.math.abs

@main def Main(): Unit = {    
    def solveNQueens(n: Int): List[List[String]] = {
        val grid = new Array[Int](n)
        var res = Vector.empty[Vector[Int]]
    
        // c1 : left , c2 : right
        def loop(row: Int, acc: Vector[Int], ys: Set[Int], c1: Set[Int], c2: Set[Int]): Unit = {
            //println(s"Row:$row $acc $ys $c1 $c2")
            if row >= n then {
                if acc.length == n then {
                    res :+= acc
                }
                return
            }            
            for {
                i <- 0 until n 
                cor1 = row + i   // left diag
                cor2 = i - row  // right diag
                if !ys(i) && !c1(cor1) && !c2(cor2)
            } do {                
                loop(row+1, acc :+ i, ys+i, c1 + cor1, c2 + cor2)
            }
        }

        loop(0, Vector.empty, Set.empty, Set.empty, Set.empty)
        def convertString(vs: Vector[Int]): List[String] = {
            val con = {(a: Int) => 
                var s = ""
                for i <- 0 until vs.length do {
                    if i == a then s += "Q" else s += "."
                }    
                s
            }
            vs.map(con).toList
        }
        res.map(convertString).toList
    }

    val r = solveNQueens(4)
    r.foreach { v => 
        println(v.mkString("\n"))
        println("===================================")
    }
}

