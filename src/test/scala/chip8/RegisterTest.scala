package chip8

import org.scalatest.FlatSpec

class RegisterTest extends FlatSpec {

   "Adding two Registers" should "return a new Register with both added" in {
     val reg1 = Register(1)
     val reg2 = Register(2)
     assert(reg1 + reg2 === Register(3))
   }

  "Subtracting two Registers" should "return a new Register with both subtracted" in {
    val reg1 = Register(2)
    val reg2 = Register(1)
    assert(reg1 - reg2 === Register(1))
  }

  "Shifting left two Registers" should "return a new Register with both shifted left" in {
    val reg1 = Register(2)
    val reg2 = Register(1)
    assert((reg1 << reg2) === Register(4))
  }

  "Shifting right two Registers" should "return a new Register with both shifted right" in {
    val reg1 = Register(2)
    val reg2 = Register(1)
    assert((reg1 >> reg2) === Register(1))
  }

  "And'ing two Registers" should "return a new Register with both and'ed" in {
    val reg1 = Register(5)
    val reg2 = Register(6)
    assert((reg1 & reg2) === Register(4))
  }

  "Or'ing two Registers" should "return a new Register with both or'ed" in {
    val reg1 = Register(2)
    val reg2 = Register(1)
    assert((reg1 | reg2) === Register(3))
  }

  "Inverting'ing two Registers" should "return a new Register with both inverted'ed" in {
    val reg1 = Register(5)
    val reg2 = Register(1)
    assert((reg1 ^ reg2) === Register(4))
  }
 }
