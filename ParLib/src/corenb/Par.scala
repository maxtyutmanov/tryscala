package corenb

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

object Par {
  def run[A](es: ExecutorService)(p: Par[A]) : A = {
    val ref = new AtomicReference[Either[Exception, A]]
    val latch = new CountDownLatch(1)
    
    p(es) { res => ref.set(res); latch.countDown }
    
    latch.await
    ref.get match {
      case Left(e) => throw e
      case Right(a) => a
    }
  }
  
  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def continueWith(cb: A => Unit): Unit =
        cb(a)
    }
    
  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def continueWith(cb: A => Unit): Unit =
        runtask(es)(a(es).continueWith(cb))
    }
    
  def runtask(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def continueWith(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => runtask(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => runtask(es)(cb(f(a, b)))
          }
        }
        
        p(es).continueWith(a => combiner ! Left(a))
        p2(es).continueWith(b => combiner ! Right(b))
      }
    }
}