

trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](x: A, xs: List[A]) extends List[A]

object List {

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  // FoldRight
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // Ex 3.09
  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => 1 + b)

  //EX 3.10

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Ex 3.11
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((a, _) => 1 + a)

  def sum3(as: List[Int]) = foldLeft(as, 0)(_ + _)

  def product3(as: List[Int]) = foldLeft(as, 1.0)(_ * _)

  //Ex 3.12
  def reverse[A] (l:List[A]):List[A] = foldLeft(l,List[A]())((a,b)=>Cons(b,a))


  def apply[A](args: A*): List[A] =
    if (args.isEmpty) Nil
    else Cons(args.head, apply(args.tail: _*))
}


val a = List(1, 2, 2, 2, 3, 4)
val b = List("a", "b", "c")
val C = List()

List.length(a)
List.length(b)
List.length(C)

List.length2(a)
List.sum2(a) //using foldRight
List.sum3(a) //using foldLeft
List.product3(a) //using foldLeft

val d = List.reverse(a)