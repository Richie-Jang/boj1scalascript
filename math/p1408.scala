@main def Main(): Unit = {

    def getTime(): Array[Int] = {
        val inp = scala.io.StdIn.readLine    
        inp.split(":").map(_.toInt)        
    }
    
    def convertSecs(t: Array[Int]): Int = {
        t(0) * 3600 + t(1) * 60 + t(2)
    }  

    val now = convertSecs(getTime())
    val start = convertSecs(getTime())
    
    def printTime(s: Int): Unit = {
        val h = s / 3600
        val m = (s / 60) % 60
        val ss = s % 60
        println(String.format("%02d:%02d:%02d", h,m,ss))
    }

    val res = if start > now then {
        start - now 
    } else {
        (24 * 3600) - (now - start)
    }
    printTime(res)
}