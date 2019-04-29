package uf4

import cats.implicits._
import cats.{Monad, Id, Monoid, Functor}
import shapeless._, ops.hlist.Selector
import annotation.implicitNotFound
import concurrent._, ExecutionContext.Implicits.global
import reflect.runtime.universe.WeakTypeTag

trait Language[Wrapper[_], Supported <: HList] {
  def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): Wrapper[A]
}

trait CliAsk[+A] {
  def promptUser: Future[A]
}

trait CliAskHlist[A <: HList] {
  type Supported = A
  def promptUser[Subtype](implicit selector : Selector[Supported, Subtype], tt: WeakTypeTag[Subtype]): Future[Subtype]
}

class FutureCliInterpreter[Supported <: HList : CliAskHlist] extends Language[Future,Supported] {
  override def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): Future[A] =
    the[CliAskHlist[Supported]].promptUser[A]
}

object FutureCliInterpreter {
  implicit val hnilAsk = new CliAskHlist[HNil] {
    def promptUser[Subtype](implicit selector : Selector[Supported, Subtype], tt: WeakTypeTag[Subtype]): Future[Subtype] =
      throw new IllegalStateException("not possible!")
  }

  implicit def hConsAsk[H, T <: HList](
    implicit
    hParser: CliAsk[H],
    tParser: CliAskHlist[T],
    tth: WeakTypeTag[H]
  ): CliAskHlist[H :: T] = new CliAskHlist[H :: T] {
    def promptUser[Subtype](implicit selector : Selector[Supported, Subtype], ttp: WeakTypeTag[Subtype]): Future[Subtype] = {
      if (ttp == tth)
        hParser.promptUser.map{_.asInstanceOf[Subtype]}
      else
        tParser.promptUser(selector.asInstanceOf[Selector[tParser.Supported,Subtype]], ttp)
    }
  }

}

// class IdMonoidInterpreter[Supported <: HList : Monoid] extends Language[Id,Supported] {
//   override def ask[A: WeakTypeTag](implicit selector : Selector[Supported, A]): A =
//     selector.apply(Monoid[Supported].empty)
// }

case class TypeA(i: String)
case class TypeB(i: String)
case class TypeC(i: String)
case class TypeD(i: String)
case class TypeE(i: String)
case class TypeF(i: String)
case class TypeG(i: String)
case class TypeH(i: String)
case class TypeI(i: String)
case class TypeJ(i: String)
case class TypeK(i: String)
case class TypeL(i: String)
case class TypeM(i: String)
case class TypeN(i: String)
case class TypeO(i: String)
case class TypeP(i: String)
case class TypeQ(i: String)
case class TypeR(i: String)
case class TypeS(i: String)
case class TypeT(i: String)
case class TypeU(i: String)
case class TypeV(i: String)
case class TypeW(i: String)
case class TypeX(i: String)
case class TypeY(i: String)
case class TypeZ(i: String)

case class OtherTypeA(i: String)
case class OtherTypeB(i: String)
case class OtherTypeC(i: String)
case class OtherTypeD(i: String)
case class OtherTypeE(i: String)
case class OtherTypeF(i: String)
case class OtherTypeG(i: String)
case class OtherTypeH(i: String)
case class OtherTypeI(i: String)
case class OtherTypeJ(i: String)
case class OtherTypeK(i: String)
case class OtherTypeL(i: String)
case class OtherTypeM(i: String)
case class OtherTypeN(i: String)
case class OtherTypeO(i: String)
case class OtherTypeP(i: String)
case class OtherTypeQ(i: String)
case class OtherTypeR(i: String)
case class OtherTypeS(i: String)
case class OtherTypeT(i: String)
case class OtherTypeU(i: String)
case class OtherTypeV(i: String)
case class OtherTypeW(i: String)
case class OtherTypeX(i: String)
case class OtherTypeY(i: String)
case class OtherTypeZ(i: String)

object FutureCliInstances {

  def instance[A](f: String => A): CliAsk[A] = new CliAsk[A] {
    def promptUser: Future[A] = Future.successful {scala.io.StdIn.readLine}.map(f)
  }

  implicit def cliAskString(implicit lp: LowPriority) = instance(identity)
  implicit def cliAskBoolean(implicit lp: LowPriority) = instance(_.toLowerCase.startsWith("y"))
  implicit def cliAskInt(implicit lp: LowPriority) = instance(_.toInt)


  implicit def cliAskTypeA(implicit lp: LowPriority) = instance(TypeA.apply)
  implicit def cliAskTypeB(implicit lp: LowPriority) = instance(TypeB.apply)
  implicit def cliAskTypeC(implicit lp: LowPriority) = instance(TypeC.apply)
  implicit def cliAskTypeD(implicit lp: LowPriority) = instance(TypeD.apply)
  implicit def cliAskTypeE(implicit lp: LowPriority) = instance(TypeE.apply)
  implicit def cliAskTypeF(implicit lp: LowPriority) = instance(TypeF.apply)
  implicit def cliAskTypeG(implicit lp: LowPriority) = instance(TypeG.apply)
  implicit def cliAskTypeH(implicit lp: LowPriority) = instance(TypeH.apply)
  implicit def cliAskTypeI(implicit lp: LowPriority) = instance(TypeI.apply)
  implicit def cliAskTypeJ(implicit lp: LowPriority) = instance(TypeJ.apply)
  implicit def cliAskTypeK(implicit lp: LowPriority) = instance(TypeK.apply)
  implicit def cliAskTypeL(implicit lp: LowPriority) = instance(TypeL.apply)
  implicit def cliAskTypeM(implicit lp: LowPriority) = instance(TypeM.apply)
  implicit def cliAskTypeN(implicit lp: LowPriority) = instance(TypeN.apply)
  implicit def cliAskTypeO(implicit lp: LowPriority) = instance(TypeO.apply)
  implicit def cliAskTypeP(implicit lp: LowPriority) = instance(TypeP.apply)
  implicit def cliAskTypeQ(implicit lp: LowPriority) = instance(TypeQ.apply)
  implicit def cliAskTypeR(implicit lp: LowPriority) = instance(TypeR.apply)
  implicit def cliAskTypeS(implicit lp: LowPriority) = instance(TypeS.apply)
  implicit def cliAskTypeT(implicit lp: LowPriority) = instance(TypeT.apply)
  implicit def cliAskTypeU(implicit lp: LowPriority) = instance(TypeU.apply)
  implicit def cliAskTypeV(implicit lp: LowPriority) = instance(TypeV.apply)
  implicit def cliAskTypeW(implicit lp: LowPriority) = instance(TypeW.apply)
  implicit def cliAskTypeX(implicit lp: LowPriority) = instance(TypeX.apply)
  implicit def cliAskTypeY(implicit lp: LowPriority) = instance(TypeY.apply)
  implicit def cliAskTypeZ(implicit lp: LowPriority) = instance(TypeZ.apply)


  implicit def cliAskOtherTypeA(implicit lp: LowPriority) = instance(OtherTypeA.apply)
  implicit def cliAskOtherTypeB(implicit lp: LowPriority) = instance(OtherTypeB.apply)
  implicit def cliAskOtherTypeC(implicit lp: LowPriority) = instance(OtherTypeC.apply)
  implicit def cliAskOtherTypeD(implicit lp: LowPriority) = instance(OtherTypeD.apply)
  implicit def cliAskOtherTypeE(implicit lp: LowPriority) = instance(OtherTypeE.apply)
  implicit def cliAskOtherTypeF(implicit lp: LowPriority) = instance(OtherTypeF.apply)
  implicit def cliAskOtherTypeG(implicit lp: LowPriority) = instance(OtherTypeG.apply)
  implicit def cliAskOtherTypeH(implicit lp: LowPriority) = instance(OtherTypeH.apply)
  implicit def cliAskOtherTypeI(implicit lp: LowPriority) = instance(OtherTypeI.apply)
  implicit def cliAskOtherTypeJ(implicit lp: LowPriority) = instance(OtherTypeJ.apply)
  implicit def cliAskOtherTypeK(implicit lp: LowPriority) = instance(OtherTypeK.apply)
  implicit def cliAskOtherTypeL(implicit lp: LowPriority) = instance(OtherTypeL.apply)
  implicit def cliAskOtherTypeM(implicit lp: LowPriority) = instance(OtherTypeM.apply)
  implicit def cliAskOtherTypeN(implicit lp: LowPriority) = instance(OtherTypeN.apply)
  implicit def cliAskOtherTypeO(implicit lp: LowPriority) = instance(OtherTypeO.apply)
  implicit def cliAskOtherTypeP(implicit lp: LowPriority) = instance(OtherTypeP.apply)
  implicit def cliAskOtherTypeQ(implicit lp: LowPriority) = instance(OtherTypeQ.apply)
  implicit def cliAskOtherTypeR(implicit lp: LowPriority) = instance(OtherTypeR.apply)
  implicit def cliAskOtherTypeS(implicit lp: LowPriority) = instance(OtherTypeS.apply)
  implicit def cliAskOtherTypeT(implicit lp: LowPriority) = instance(OtherTypeT.apply)
  implicit def cliAskOtherTypeU(implicit lp: LowPriority) = instance(OtherTypeU.apply)
  implicit def cliAskOtherTypeV(implicit lp: LowPriority) = instance(OtherTypeV.apply)
  implicit def cliAskOtherTypeW(implicit lp: LowPriority) = instance(OtherTypeW.apply)
  implicit def cliAskOtherTypeX(implicit lp: LowPriority) = instance(OtherTypeX.apply)
  implicit def cliAskOtherTypeY(implicit lp: LowPriority) = instance(OtherTypeY.apply)
  implicit def cliAskOtherTypeZ(implicit lp: LowPriority) = instance(OtherTypeZ.apply)


}

object Tagless extends App {

  def addAndIncrement[F[_]: Monad](a: Int)(interpreter: Language[F,String :: Int :: HNil]): F[Int] = {
    import interpreter._
    for {
      blah <- ask[Int]
      blah2 <- ask[String]
    } yield (blah)
  }

  def askAndAdd[F[_]: Functor](a: Int)(interpreter: Language[F,Int :: HNil]): F[Int] = {
    import interpreter._
    ask[Int].map{_ + a}
  }

  def stupidProgram[F[_]: Monad](interpreter: Language[F,OtherTypeA :: OtherTypeB :: OtherTypeC :: OtherTypeD :: OtherTypeE :: OtherTypeF :: OtherTypeG :: OtherTypeH :: OtherTypeI :: OtherTypeJ :: OtherTypeK :: OtherTypeL :: OtherTypeM :: OtherTypeN :: OtherTypeO :: OtherTypeP :: OtherTypeQ :: OtherTypeR :: OtherTypeS :: OtherTypeT :: OtherTypeU :: OtherTypeV :: OtherTypeW :: OtherTypeX :: OtherTypeY :: OtherTypeZ :: TypeA :: TypeB :: TypeC :: TypeD :: TypeE :: TypeF :: TypeG :: TypeH :: TypeI :: TypeJ :: TypeK :: TypeL :: TypeM :: TypeN :: TypeO :: TypeP :: TypeQ :: TypeR :: TypeS :: TypeT :: TypeU :: TypeV :: TypeW :: TypeX :: TypeY :: TypeZ :: HNil]): F[TypeZ] = {
    import interpreter._
    for {
      blah <- ask[TypeZ]
      blah2 <- ask[OtherTypeA]
    } yield (blah)
  }

  // implicit val monStack: Monoid[String :: Int :: HNil] = new Monoid[String :: Int :: HNil] {
  //   def empty = "" :: 0 :: HNil
  //   def combine(a: String :: Int :: HNil,b: String :: Int :: HNil) = ??? 
  // }
  import FutureCliInterpreter._
  import FutureCliInstances._

  implicit val cliAskBoolean2 = new CliAsk[Boolean] {
    def promptUser: Future[Boolean] = Future.successful {scala.io.StdIn.readLine.toLowerCase.startsWith("y")}
  }  

  // addAndIncrement(1)(new FutureCliInterpreter).onComplete{println}

 stupidProgram(new FutureCliInterpreter).onComplete{println}

//  askAndAdd(12)(new FutureCliInterpreter).onComplete{println}

}
