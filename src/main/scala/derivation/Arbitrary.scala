package derivation

import shapeless._
import scala.util.Random

sealed trait Animal
case class Cat(name: String, fish: Int) extends Animal
case class Dog(friend: Animal, bones: Option[Int]) extends Animal

object Arbitrary {
  def apply[T](implicit a: Arbitrary[T]) = a
  def from[T](a: => T) = new Arbitrary[T] { def value = a }
  
  implicit val arbitraryInt = Arbitrary from Random.nextInt(100)
  implicit val arbitraryString = Arbitrary from Random.alphanumeric.take(10).mkString("")

  // Base case for coproducts
  implicit def arbitraryCLast[H](implicit h: Arbitrary[H]): Arbitrary[H :+: CNil] =
    Arbitrary from Inl(h.value)

  // Induction step for coproducts
  implicit def arbitraryCCons[H, T <: Coproduct](implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :+: T] =
    Arbitrary from (if(Random.nextBoolean) Inl(h.value) else Inr(t.value))

  // Base case for products
  implicit val arbitraryHNil: Arbitrary[HNil] =
    Arbitrary from HNil

  // Induction step for products
  implicit def arbitraryHCons[H, T <: HList](implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :: T] =
    Arbitrary from (h.value :: t.value)

  implicit def arbitraryGeneric[T, R](implicit gen: Generic.Aux[T, R], arb: Lazy[Arbitrary[R]]): Arbitrary[T] =
    Arbitrary from gen.from(arb.value.value)
}

trait Arbitrary[T] {
  def value: T
}

object User extends App {
  (0 until 10).foreach(_ => println(Arbitrary[Animal].value))
}
