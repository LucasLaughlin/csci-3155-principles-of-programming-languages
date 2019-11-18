package edu.colorado.csci3155.project1

import org.scalatest.FunSuite

class StackMachineTest extends FunSuite {
    test("stack machine test 1") {
        val lst1 = List(PushI(2.5), PushI(3.5), AddI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(f == 6.0)
    }

    test("stack machine test 2") {
        val lst1 = List(PushI(2.5), PushI(3.5), AddI, ExpI, LogI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(math.abs(f - 6.0) <= 1e-05)
    }

    test("stack machine test 5") {
        val lst1 = List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI)
        val lst2 = List(PushI(1.0), PushI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI)
        val f = StackMachineEmulator.emulateStackMachine(lst3)
        assert(math.abs(f - 3.50287) <= 1E-04)
    }


}
