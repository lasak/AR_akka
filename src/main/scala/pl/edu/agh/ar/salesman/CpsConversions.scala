package pl.edu.agh.ar.salesman

object CpsConversions {

  import scala.collection.IterableLike
  import scala.util.continuations._

  implicit def cpsIterable[A, Repr](xs: IterableLike[A, Repr]) = new {
    def cps = new {
      def foreach[B](f: A => Any@cpsParam[Unit, Unit]): Unit@cpsParam[Unit, Unit] = {
        val it = xs.iterator
        while(it.hasNext) f(it.next)
      }
    }
  }
}
