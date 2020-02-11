import java.util.concurrent.ExecutorService
import java.util.concurrent.Future


package object core {
  type Par[A] = ExecutorService => Future[A]
}