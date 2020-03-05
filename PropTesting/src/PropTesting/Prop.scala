package PropTesting

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

//trait Prop {
//  def check: Boolean
//  
//  def &&(other: Prop): Prop = {
//    val self = this;
//    new Prop {
//      def check = this.check && other.check 
//    }
//  }
//}