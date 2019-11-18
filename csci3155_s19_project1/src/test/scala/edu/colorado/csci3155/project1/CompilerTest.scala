package edu.colorado.csci3155.project1
import org.scalatest.FunSuite


class CompilerTest extends FunSuite {
    test ("simple expression 1") {
        val e = Const(1.0)
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushI(1.0)))
    }

    test("simple expression 2") {
        val e = Plus(Const(1.0), Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushI(1.0), PushI(2.5), AddI))
    }

    test("simple expression 3"){
        val e = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI))
    }

    test("simple expression 4") {
        val e1 = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val e2 = Plus(Const(1.0), Const(2.5))
        val e3 = Log(Plus(Exp(e1), Exp(e2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e3)
        //println("----")
        //lst.foreach(println)
        //println("----")
        val lst1 = List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI)
        val lst2 = List(PushI(1.0), PushI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI)
        assert(lst == lst3)
    }

}
