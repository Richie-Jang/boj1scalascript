package geom.p1002

import java.io.BufferedReader
import java.io.InputStreamReader

@main def Main(): Unit = {

    def solve(s: String): Int = {
        val ss = s.split(' ').map(_.toInt)
        val x1 = ss(0)
        val y1 = ss(1)
        val r1 = ss(2)
        val x2 = ss(3)
        val y2 = ss(4)
        val r2 = ss(5)

        if x1 == x2 && y1 == y2 then {
            // same position
            if r1 == r2 then {
                return -1
            }            
            return 0
        }
        val rDist = (r1 + r2) * (r1 + r2)
        val pDist = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
        
        if rDist == pDist then return 1
        if rDist < pDist then return 0
        val dDist = (r1 - r2) * (r1 - r2)        
        if pDist == dDist then return 1
        if pDist < dDist then return 0
        2
    }

    val br = new BufferedReader(new InputStreamReader(System.in))
    val t = br.readLine().toInt
    
    val res = (1 to t).map { a =>
        solve(br.readLine().trim)
    }    
    br.close()

    println(res.mkString("\n"))

}
