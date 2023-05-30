package greedy.p2812

@main def Main(): Unit = {
    
    import scala.io.StdIn
    
    val (n,k) = {
        val r = StdIn.readLine().split(' ').map(_.toInt)
        r(0) -> r(1)
    }

    val s = StdIn.readLine()

    val len = s.length()
    var stack = List.empty[Char]

    def loop(index: Int, count: Int, isContinue: Boolean = false): Unit = {
        if index >= len then {
            for remained <- count-1 to 0 by -1 do {
                stack = stack.tail
            }
            return
        }
        if count == 0 then {
            if isContinue then {
                stack = s(index) :: stack
            }

            // no reached to index end
            for i <- index+1 until len do {
                stack = s(i) :: stack
            }

            return
        }

        //println(s"$stack, $index, $count")

        val cur = s(index)
        stack match {
            case h::t =>
                if h < cur then {
                    stack = t                                        
                    loop(index, count-1, true)
                } else {
                    stack = cur :: stack
                    loop(index+1, count)
                }
            case Nil => 
                stack = cur :: stack
                loop(index+1, count)
        }
    }
        
    loop(0, k)
    println(stack.reverse.mkString(""))

}