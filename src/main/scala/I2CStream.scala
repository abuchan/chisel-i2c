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
  
  val in_queue = Module(new Queue(UInt(width=8), 16))
  val out_queue = Module(new Queue(UInt(width=8), 16))

  io.host_in <> in_queue.io.enq
  in_queue.io.deq <> bridge.io.host_in

  io.host_out <> out_queue.io.deq
  out_queue.io.enq <> bridge.io.host_out

  bridge.io.user <> master.io.user
  
  io.bus <> master.io.bus
}

class I2CStreamTests(c: I2CStream) extends Tester(c) {
  poke(c.io.host_in.valid,1)

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
  poke(c.io.host_in.bits, 0x08)
  step(1)

  // Send Stop
  poke(c.io.host_in.bits, 0xFF)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(1)
  poke(c.io.host_in.bits, 0x02)
  step(1)

  poke(c.io.host_in.valid, 0)

  // Delay start and write
  step(256 * (3 + 18))
  
  //set SDA in to 1 during read to read 0xFF and NACK
  poke(c.io.bus.sda.in, 1)
  
  // Delay write and stop, and extra time
  step(256 * (18 + 3 + 5))
  
  // Start reading out values from host_out queue
  poke(c.io.host_out.ready, 1)
  step(1)

  // Expect ACK from write
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0xFF)
  step(1)
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0x04)
  step(1)

  // Expect read 0xFF (which is escaped as 0xFF 0xFF) and NACK from read
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0xFF)
  step(1)
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0xFF)
  step(1)
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0xFF)
  step(1)
  expect(c.io.host_out.valid, 1)
  expect(c.io.host_out.bits, 0x05)
  step(1)
}
