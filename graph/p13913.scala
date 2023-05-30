
object p13913 {

    import scala.io.StdIn
    import scala.collection.mutable
    import scala.util.control.Breaks.*

    case class Info(list: List[Int], value: Int)

    def main(args: Array[String]): Unit = {
        val MAX = 100_000
        val (n,k) = {
            val v = StdIn.readLine().split("\\s+").map(_.toInt)
            v(0) -> v(1)
        }

        val q = mutable.Queue.empty[Info]
        val visits = mutable.Set.empty[Int]

        q.enqueue(Info(List(n), n))
        visits += n

        breakable {
            while q.nonEmpty do {
                val cur = q.dequeue()
                //println(cur)
                if cur.value == k then {
                    println(cur.list.length-1)
                    println(cur.list.reverse.mkString(" "))
                    break()
                }
                var newValue = cur.value - 1
                if newValue >= 0 && !visits(newValue) then 
                    q.enqueue(Info(newValue::cur.list, newValue))
                    visits += newValue
                newValue = cur.value + 1                
                if newValue <= MAX && !visits(newValue) then
                    q.enqueue(Info(newValue::cur.list, newValue))
                    visits += newValue
                newValue = cur.value * 2
                if newValue <= MAX && !visits(newValue) then 
                    q.enqueue(Info(newValue::cur.list, newValue))
                    visits += newValue
            }
        }
    }
}