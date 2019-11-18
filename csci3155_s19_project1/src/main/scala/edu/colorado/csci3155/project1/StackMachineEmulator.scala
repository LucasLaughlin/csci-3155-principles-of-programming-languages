package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] =


        ins match{
            case PushI(f: Double) => f:: stack
            case PopI if(stack.length>0) => stack.tail
            case LogI if(stack.length>0 && stack.head>0) => math.log(stack.head)::stack.tail
            case SinI if(stack.length>0) => math.sin(stack.head) :: stack.tail
            case CosI if(stack.length>0) => math.cos(stack.head):: stack.tail
            case ExpI if(stack.length>0) => scala.math.pow(scala.math.E,stack.head):: stack.tail
            case AddI if(stack.length>1) => stack.head + stack.tail.head::stack.tail.tail
            case SubI if(stack.length>1) => stack.tail.head  - stack.head::stack.tail.tail
            case MultI if(stack.length>1) => stack.head * stack.tail.head::stack.tail.tail
            case DivI if(stack.length>1) => stack.tail.head / stack.head::stack.tail.tail
            case _ => throw new IllegalArgumentException
        }


    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double = {
        val stack = List[Double]()
        instructionList.foldLeft(stack){(stack, i) => (emulateSingleInstruction(stack, i))}.head

    }
}