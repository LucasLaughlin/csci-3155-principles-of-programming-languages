package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {

        e match {
            case Const(f) => PushI(f)::Nil
            case Exp(f) => compileToStackMachineCode(f):+ExpI
            case Log(f) => compileToStackMachineCode(f):+LogI
            case Cosine(f) => compileToStackMachineCode(f):+CosI
            case Sine(f) => compileToStackMachineCode(f):+SinI
            case Plus(f1, f2) => compileToStackMachineCode(f1):::compileToStackMachineCode(f2):::List(AddI)
            case Minus(f1, f2) => compileToStackMachineCode(f1):::compileToStackMachineCode(f2):::List(SubI)
            case Mult(f1, f2) =>  compileToStackMachineCode(f1):::compileToStackMachineCode(f2):::List(MultI)
            case Div(f1, f2) => compileToStackMachineCode(f1):::compileToStackMachineCode(f2):::List(DivI)
        }
    }
}

