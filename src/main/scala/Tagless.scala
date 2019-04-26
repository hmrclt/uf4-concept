import cats.implicits._
import cats.Monad
import cats.Id
import shapeless._
import shapeless.ops.hlist.Selector
import scala.annotation.implicitNotFound
import cats.Monoid
import scala.concurrent._
import reflect.runtime.universe.WeakTypeTag

object Tagless extends App {

  trait Language[Wrapper[_], Supported <: HList] {
    def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): Wrapper[A]
  }

  trait CliAsk[A <: HList] {
    type Supported = A
    def promptUser[Subtype](implicit selector : Selector[Supported, Subtype], tt: WeakTypeTag[Subtype]): Future[Subtype] = ???
  }

  def futureCliInterpreter[Supported <: HList : CliAsk] = new Language[Future,Supported] {
    override def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): Future[A] =
      the[CliAsk[Supported]].promptUser[A]
  }

  def idMonoidInterpreter[Supported <: HList : Monoid] = new Language[Id,Supported] {
    override def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): A =
      selector.apply(Monoid[Supported].empty)
  }

  def addAndIncrement[F[_]: Monad](a: Int)(interpreter: Language[F,String :: Int :: HNil]): F[Int] = {
    import interpreter._
    for {
      blah <- ask[Int]
      blah2 <- ask[String]
    } yield (blah)
  }

  implicit val cliAsker: CliAsk[String :: Int :: HNil] = new CliAsk[String :: Int :: HNil] {
    override def promptUser[Subtype](implicit selector : Selector[Supported, Subtype], tt: WeakTypeTag[Subtype]): Future[Subtype] = ???
  }

  implicit val monStack: Monoid[String :: Int :: HNil] = new Monoid[String :: Int :: HNil] {
    def empty = "" :: 0 :: HNil
    def combine(a: String :: Int :: HNil,b: String :: Int :: HNil) = ??? 
  }

  println(addAndIncrement(1)(idMonoidInterpreter[String :: Int :: HNil]))

}
