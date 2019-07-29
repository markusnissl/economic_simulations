package ecosim

object TypeClassExamples extends App {
  
  case class Person(name: String, age: Int)
  val ps = List(Person("A",1), Person("B",3), Person("B",2))
  println(ps.sorted(new Ordering[Person] {
    override def compare(x: Person, y: Person) = x.age - y.age
  }))
  println(ps.sorted(Ordering.by((p: Person) => p.age)))
  
  implicit object PersonOrdering extends Ordering[Person] {
    override def compare(x: Person, y: Person) = x.age - y.age
  }
  println(ps.sorted)
  
  List((1,2),(3,4)).sorted
  
}
