

case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })
    
  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))
    
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => sb.map(b => f(a,b)))
}

case object State {
  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))
    
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](Nil:List[A]))((h, acc) => h.map2(acc)(_::_))
    
  def get[S]: State[S, S] = State(s => (s, s))
  
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  
  def modify[S](f: S => S): State[S, Unit] = for {
    initial <- get
    _ <- set(f(initial))
  } yield ()
}