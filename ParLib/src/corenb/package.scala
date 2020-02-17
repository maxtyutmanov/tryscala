import java.util.concurrent.ExecutorService

package object corenb {
  abstract class Future[+A] {
    private[corenb] final def apply(k: Either[Exception, A] => Unit): Unit = {
      try {
        continueWith(a => k(Right(a)))
      }
      catch {
        case e: Exception => k(Left(e))
      }
    }
    
    private[corenb] def continueWith(k: A => Unit): Unit
  }
  
  type Par[+A] = ExecutorService => Future[A]
  
  //def withErrorHandling[A](f: A => Unit): Either[Exception, A] => Unit
}