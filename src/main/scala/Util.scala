package I2C

import Chisel._

class Counter(abs_max: UInt) extends Module {
  val io = new Bundle {
    val en = Bool(INPUT)
    val reset = Bool(INPUT)
    val count = UInt(OUTPUT, abs_max.getWidth)
    val max = UInt(INPUT, abs_max.getWidth)
    val top = Bool(OUTPUT)
  }

  def this(abs_max: Int) = this(UInt(abs_max))

  val x = Reg(init = UInt(0, abs_max.getWidth))

  io.count := x
  io.top := x === io.max

  when (io.reset) {x := UInt(0)}
  .elsewhen (io.en) {x := Mux(io.top, UInt(0), x + UInt(1))}
}

