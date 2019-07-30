package ecosim.deep

object Interpreter {
  import IR.Predef._
  
  case class Assignment[V: CodeType](v: Variable[V], arg: V)(implicit val V: CodeType[V])
  
  def apply[A: CodeType](algo: Algo[A], ass: List[Assignment[_]]): A = algo match {
    case ScalaCode(cde) => bindAll(ass, cde).evalClosed
    case fe: Foreach[b] =>
      import fe.E
      val ls = bindAll(ass, fe.ls).evalClosed
      val v = Variable[b]
      val al = fe.f(v)
      ls.foreach { e => apply(al, Assignment(v,e) :: ass) }
  }
  
  def bindAll[A: CodeType](ass: List[Assignment[_]], cde: OpenCode[A]): Bound[A] = ass match {
    case Nil => BoundNil(cde)
    case (as: Assignment[v]) :: ass =>
      import as._
      BoundCons(as.v, as.arg, bindAll(ass, cde))
  }
  
  sealed abstract class Bound[A] {
    type T
    implicit val T: CodeType[T]
    def cde: OpenCode[T]
    def adapt: T => A
    def evalClosed: A = adapt(cde.unsafe_asClosedCode.run)
  }
  case class BoundCons[V: CodeType, A: CodeType](v: Variable[V], arg: V, rest: Bound[A]) extends Bound[A] {
    type T = V => rest.T
    val T: CodeType[T] = implicitly
    def cde = code"($v: V) => ${rest.cde}"
    val adapt = (f: T) => rest.adapt(f(arg))
  }
  case class BoundNil[A: CodeType](c: OpenCode[A]) extends Bound[A] {
    type T = A
    val T: CodeType[T] = implicitly
    def cde = c
    def adapt: T => A = identity
  }
  
}

object InterpreterTest extends App {
  import IR.Predef._
  
  val lsc = code"List(1,2,3)"
  
  println(Interpreter(ScalaCode(lsc), Nil))
  
  println(Interpreter(Foreach(lsc, (x: Variable[Int]) => ScalaCode(code"println($x + 1)")), Nil))
  
}
