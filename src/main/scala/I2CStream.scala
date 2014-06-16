package I2C

import Chisel._

class I2CStreamIO extends Bundle{
  val host_in = Decoupled(UInt(width=8)).flip
  val host_out = Decoupled(UInt(width=8))

  val bus = new I2CBusIO()
}

class I2CStream(synth: Boolean = true)  extends Module {
  val io = new I2CStreamIO()

  val bridge = Module(new I2CBridge())
  val master = Module(new I2CMaster())
  
  if (synth) {
    io.host_in <> bridge.io.host_in
    io.host_out <> bridge.io.host_out
  } else {
    val in_queue = Module(new Queue(UInt(width=8), 32))
    val out_queue = Module(new Queue(UInt(width=8), 32))
    
    io.host_in <> in_queue.io.enq
    in_queue.io.deq <> bridge.io.host_in

    io.host_out <> out_queue.io.deq
    out_queue.io.enq <> bridge.io.host_out
  }

  bridge.io.user <> master.io.user
  
  io.bus <> master.io.bus
}

class I2CStreamTests(c: I2CStream) extends Tester(c) {

  def write_bytes(vals: List[Int]) = {
    for (i <- 0 until vals.length) {
      poke(c.io.host_in.bits, vals(i))
      step(1)
    }
  }

  def expect_bytes(vals: List[Int]) = {
    for (i <- 0 until vals.length) {
      expect(c.io.host_out.valid, 1)
      expect(c.io.host_out.bits, vals(i))
      step(1)
    }
  }

  poke(c.io.host_in.valid,1)

  // Set divider to 1
  write_bytes(List(0xFF, 0x00, 0x01))
  
  // Send start
  write_bytes(List(0xFF, 0x02, 0x01))

  // Write 0x78, 0x02
  write_bytes(List(0x78, 0x02))
  
  // Repeated Start
  write_bytes(List(0xFF, 0x02, 0x01))
  
  // 2x Read 
  write_bytes(List(0xFF, 0x02, 0x08))
  write_bytes(List(0xFF, 0x02, 0x08))

  // Send Stop
  write_bytes(List(0xFF, 0x02, 0x02))

  poke(c.io.host_in.valid, 0)

  // Delay start, 2 write, repeated start, and one read
  step(256 * (3 + 2*20 + 3 + 18))
  
  //set SDA in to 1 during second read to read 0xFF and NACK
  poke(c.io.bus.sda.in, 1)
  
  // Delay read and stop, and extra time
  step(256 * (20 + 3 + 5))
  
  // Start reading out values from host_out queue
  poke(c.io.host_out.ready, 1)
  step(1)

  // Expect 2 ACK from write
  expect_bytes(List(0xFF, 0x04))
  expect_bytes(List(0xFF, 0x04))

  // Expect read 0x00 ACK
  expect_bytes(List(0x00, 0xFF, 0x04))

  // Expect read 0xFF (which is escaped as 0xFF 0xFF) and NACK from read
  expect_bytes(List(0xFF, 0xFF, 0xFF, 0x05))
}
