package I2C

import Chisel._

class I2CStreamIO extends Bundle{
  val host_in = Decoupled(UInt(width=8)).flip
  val host_out = Decoupled(UInt(width=8))

  val bus = new I2CBusIO()
}

class I2CStream extends Module {
  val io = new I2CStreamIO()

  val bridge = Module(new I2CBridge())
  val master = Module(new I2CMaster())

  io.host_in <> bridge.io.host_in
  io.host_out <> bridge.io.host_out

  bridge.io.user <> master.io.user
  
  io.bus <> master.io.bus
}

class I2CStreamTests(c: I2CStream) extends Tester(c) {
  poke(c.io.host_in.valid,1)
  step(1)

  // Set divider to 1
  poke(c.io.host_in.bits, 0xFF)
  step(1)
  poke(c.io.host_in.bits, 0x00)
  step(1)
  poke(c.io.host_in.bits, 0x01)
  step(1)
  
  // Send start
  poke(c.io.host_in.bits, 0xFF)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(1)
  poke(c.io.host_in.bits, 0x01)
  step(1)

  // Write 0x42
  poke(c.io.host_in.bits, 0x42)
  step(1)
  
  // Read
  poke(c.io.host_in.bits, 0xFF)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(1)
  poke(c.io.host_in.bits, 0x04)
  step(1)

  // Send Stop
  poke(c.io.host_in.bits, 0xFF)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(256 * 10 * 5)
}
