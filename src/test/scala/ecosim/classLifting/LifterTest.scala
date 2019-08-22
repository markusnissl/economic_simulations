//package ecosim.classLifting
//
//import ecosim.deep.{Foreach, Forever, IR, If, IfElse, LetBinding, ScalaCode, Wait, NoOp}
//import org.scalatest.FunSuite
//import IR.Predef._
//
//class LifterTest extends FunSuite{
//  val lifter = new Lifter()
//  test("Lifter.liftCode") {
//    var cde = code"()"
//    var liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[NoOp])
//    cde = code"""println("test")"""
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[ScalaCode[_]])
//    cde = code"var a = 5; ()"
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[LetBinding[_,_]])
//    cde = code"var a = 5; println(a)"
//    //TODO make it a bit more clear why and how its like this
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[LetBinding[_,_]])
//    assert(liftedCde.asInstanceOf[LetBinding[_,_]].value.isInstanceOf[ScalaCode[_]])
//    assert(liftedCde.asInstanceOf[LetBinding[_,_]].rest.isInstanceOf[LetBinding[_,_]])
//    cde = code"val a = List(1,2,3); a.foreach(x => println(x)); ()"
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[LetBinding[_,_]])
//    assert(liftedCde.asInstanceOf[LetBinding[_,_]].rest.isInstanceOf[Foreach[_,_]])
//    cde = code"while(true) println(5)"
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[Forever])
//    cde = code"if(5 == 3) println(5) else println(6)"
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[IfElse[_]])
//    cde = code"val a = List(1,2,3); a.map(x => {if(x == 5) println(x); x}); ()"
//    assertThrows[Exception] {
//      lifter.liftCode(cde, null, null)
//    }
//    cde = code"SpecialInstructions.waitTurns(3); ()"
//    liftedCde = lifter.liftCode(cde, null, null)
//    assert(liftedCde.isInstanceOf[Wait])
//  }
//}
