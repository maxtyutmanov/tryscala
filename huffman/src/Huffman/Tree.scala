package Huffman

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Nil extends Tree[Nothing]

sealed trait Turn
case object Left extends Turn
case object Right extends Turn

object Tree {
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  
  def size[A](t: Tree[A]): Int = 
    fold(t)(_ => 1)((accL, accR) => 1 + accL + accR)
  
  def max(t: Tree[Int]): Int = 
    fold(t)(x => x)((accL, accR) => accL max accR)
  
  def depth[A](t: Tree[A]): Int = 
    fold(t)(_ => 0)((dl, dr) => 1 + (dl max dr))
    
  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = 
    fold(t)(x => Leaf(f(x)): Tree[B])((tl, tr) => Branch(tl, tr))
    
  def mapPath[A,B](t: Tree[A])(f: (A, List[Turn]) => B) : Tree[B] = 
    mapPathInternal(t)(scala.Nil:List[Turn])(f)
    
  private def mapPathInternal[A,B](t: Tree[A])(path: List[Turn])(f: (A, List[Turn]) => B) : Tree[B] = t match {
    case Branch(l, r) => Branch(mapPathInternal(l)(Left::path)(f), mapPathInternal(r)(Right::path)(f))
    case Leaf(x) => Leaf(f(x, path))
    case Nil => Nil
  }
}