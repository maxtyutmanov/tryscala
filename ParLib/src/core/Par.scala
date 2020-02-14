package core

import java.util.concurrent.{Executors, ExecutorService, Future, Callable}
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeoutException

object Par {
  
  def unit[A](a: A): Par[A] = (es) => UnitFuture(a)
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => { 
      MapFuture(a(es), b(es), f)
    }
    
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))
    
  def fork[A](a: => Par[A]): Par[A] = {
    (es: ExecutorService) => {
      es.submit(new Callable[A] {
        def call = a(es).get
      })
    }
  }
  
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil:List[A]))((a, acc) => map2(a, acc)((aVal, accVal) => aVal::accVal))
    
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val mapped = parMap(as)(a => (a, f(a)))
    map(mapped)(list => list.filter(x => x._2).map(x => x._1))
  }
  
  def aggregate[A,B](as: List[A], d: B)(f: (B,A) => B)(g: (B,B) => B): Par[B] = as match {
    case Nil => unit(d)
    case h::Nil => lazyUnit(f(d, h))
    case _ => {
      val (l, r) = as.splitAt(as.length / 2)
      val pl = fork(aggregate(l, d)(f)(g))
      val pr = fork(aggregate(r, d)(f)(g))
      map2(pl, pr)(g)
    }
  }
  
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
    
  private case class MapFuture[A,B,C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    
    def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)
    def isDone = fa.isDone && fb.isDone
    def get(timeout: Long, units: TimeUnit) =
      getInternal(units.toNanos(timeout))

    def get(): C = getInternal(Long.MaxValue)

    def isCancelled(): Boolean = fa.isCancelled || fb.isCancelled
    
    def getInternal(timeoutNs: Long) : C = cache match {
      case Some(_) => cache.get
      case None => {
        val startTimeNs = System.nanoTime()
      
        val a = fa.get(timeoutNs, TimeUnit.NANOSECONDS)
        val leftNs = System.nanoTime() - startTimeNs
        // just in case. In fact, this should always be positive 
        if (leftNs <= 0)
          throw new TimeoutException()
        val b = fb.get(leftNs, TimeUnit.NANOSECONDS)
        
        val c = f(a, b)
        cache = Some(c)
        c
      }
    }
  }
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}