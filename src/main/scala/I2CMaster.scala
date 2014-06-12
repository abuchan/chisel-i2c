package I2C

import Chisel._

class I2CMasterCtrlIO extends Bundle{
  val start = Bool(INPUT)
  val stop = Bool(INPUT)
  val ack = Bool(INPUT)
}

class I2CMasterIO extends Bundle{
  val ctrl_in = new I2CMasterCtrlIO()  
  val ctrl_out = new I2CMasterCtrlIO().flip
  
  val clock_div_in = UInt(INPUT, 8)
  val clock_div_out = UInt(OUTPUT, 8)

  val data_in = Decoupled(UInt(width=8)).flip
  val data_out = Decoupled(UInt(width=8))

  val scl_in = Bool(INPUT)
  val scl_out = Bool(OUTPUT)
  val sda_in = Bool(INPUT)
  val sda_out = Bool(OUTPUT)
}

class I2CMaster extends Module {
  val io = new I2CMasterIO()

}

class I2CMasterTests(c: I2CMaster) extends Tester(c) {
  for (i <- 0 until 10) {
    poke(c.io.start, 1)
    step(512)
    expect(c.io.scl_out,1)
  }
}
