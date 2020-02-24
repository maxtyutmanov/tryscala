import java.util.concurrent.ExecutorService

package object corenb {
  abstract class Future[+A] {
    private[corenb] def apply(k: Either[Exception, A] => Unit): Unit
  }
  
  def mapEH[A](a: => A): Either[Exception, A] = {
    try {
      Right(a)
    }
    catch {
      case e: Exception => Left(e)
    }
  }
  
  type Par[+A] = ExecutorService => Future[A]
}