package I2C

import Chisel._

class I2CMasterCtrlIO extends Bundle{
  val start = Bool(INPUT)
  val stop = Bool(INPUT)
  val write = Bool(INPUT)
  val read = Bool(INPUT)
}

class PullupIO extends Bundle{
  val in = Bool(INPUT)
  val out = Bool(OUTPUT)
}

class I2CBusIO extends Bundle{
  val scl = new PullupIO()
  val sda = new PullupIO()
}

class I2CMasterUserIO extends Bundle{
  val ctrl = new I2CMasterCtrlIO()  
  val stat = new I2CMasterCtrlIO().flip
  
  val active = Bool(OUTPUT)
  val ack = Bool(OUTPUT)
  
  val clock_div_in = UInt(INPUT, width=8)
  val clock_div_out = UInt(OUTPUT, width=8)

  val data_in = UInt(INPUT, width=8)
  val data_out = UInt(OUTPUT, width=8)
}

class I2CMasterIO extends Bundle{
  val user = new I2CMasterUserIO()  
  val bus = new I2CBusIO()
}

class I2CMaster extends Module {
  val io = new I2CMasterIO()
  
  // Reg SCL and SDA to avoid glitches
  val scl_reg = Reg(init = Bool(true))
  val sda_reg = Reg(init = Bool(true))
  io.bus.scl.out := scl_reg
  io.bus.sda.out := sda_reg
  
  // Sample input lines
  val scl_in_reg = Reg(next = io.bus.scl.in)
  val sda_in_reg = Reg(next = io.bus.sda.in)

  val reset_counters = Bool()
  reset_counters := Bool(false)

  // Counter for producing SCL clock period
  val clock_div = Reg(init=UInt(0xFF,width=8))
  val clock_max = UInt(width=16)
  clock_max := clock_div ## UInt(0,width=8)
  val clock_counter = Module(new Counter(clock_max))
  clock_counter.io.max := clock_max
  clock_counter.io.en := Bool(false)
  clock_counter.io.reset := reset_counters

  // Report MSB of clock divider
  io.user.clock_div_out := clock_div

  // Counter for number of bit transactions in each state
  val bit_max = Reg(init=UInt(0,width=5))
  val bit_counter = Module(new Counter(bit_max))
  bit_counter.io.max := bit_max
  bit_counter.io.en := clock_counter.io.top
  bit_counter.io.reset := reset_counters
  
  // Use bit_pos to index read and write bits
  val bit_pos = UInt()
  bit_pos := bit_counter.io.count >> UInt(1)

  val state_done = Bool()
  state_done := clock_counter.io.top && bit_counter.io.top

  val s_idle :: s_start :: s_wait :: s_read :: s_write :: s_stop :: Nil = Enum(UInt(),6)
  val state = Reg(init = s_idle)

  io.user.active := state != s_idle
  io.user.stat.start := state === s_start
  io.user.stat.stop := state === s_stop
  io.user.stat.write := state === s_write
  io.user.stat.read := state === s_read

  val data_vec = Vec.fill(8) {Reg(init=UInt(0,width=1))}
  io.user.data_out := data_vec.reduceRight[UInt](Cat(_,_))

  val ack_reg = Reg(init = Bool(false))
  io.user.ack := ack_reg
  
  // True when in the middle of a clock high period
  val sample = Bool()
  sample := scl_reg && clock_counter.io.count === clock_div ## UInt(0,width=7)

  when (sample && (bit_pos === UInt(8))) {
    ack_reg := !sda_in_reg
  }

  // True when at the end of the specified bit period
  def count_edge(n: Int) = {
    clock_counter.io.top && (bit_counter.io.count === UInt(n))
  }

  switch(state) {
    is (s_idle) {
      reset_counters := Bool(true)
      sda_reg := Bool(true)
      scl_reg := Bool(true)
      ack_reg := Bool(false)

      // Don't allow clock divider of zero
      unless (io.user.clock_div_in === UInt(0)) {
        clock_div := io.user.clock_div_in
      }

      when (io.user.ctrl.start) {
        state := s_start
        bit_max := UInt(2)
        sda_reg := Bool(true)
        scl_reg := Bool(true)
      }
    }
    is (s_start) {
      clock_counter.io.en := Bool(true)

      // Cause falling edge on SDA, then set SCL low
      when (count_edge(0)) {
        sda_reg := Bool(false)
      } 
      when(count_edge(1)) {
        scl_reg := Bool(false)
      }

      when (state_done) {
        state := s_wait
      }
    }
    is (s_wait) {
      reset_counters := Bool(true)

      when (io.user.ctrl.start) {
        state := s_start
        bit_max := UInt(2)
        sda_reg := Bool(true)
        scl_reg := Bool(true)
      }
      .elsewhen (io.user.ctrl.write) {
        state := s_write
        bit_max := UInt(18)
        // Latch in data, MSB is sent first
        for (i <- 0 until 8) {
          data_vec(i) := io.user.data_in(7-i)
        }
        sda_reg := io.user.data_in(7)
        scl_reg := Bool(false)
      }
      .elsewhen (io.user.ctrl.read) {
        state := s_read
        bit_max := UInt(18)
        sda_reg := Bool(true)
        scl_reg := Bool(false)
      }
      .elsewhen (io.user.ctrl.stop) {
        state := s_stop
        bit_max := UInt(2)
        sda_reg := Bool(false)
        scl_reg := Bool(false)
      }
    }
    is (s_write) {
      clock_counter.io.en := Bool(true)
      
      // Toggle SCL and set SDA to next bit, or high when reading ACK
      when (clock_counter.io.top) {
        when(bit_pos <= UInt(8)) {
          scl_reg := ~scl_reg
        }

        when (scl_reg) {
          when (bit_pos < UInt(7)) {
            sda_reg := data_vec(bit_pos + UInt(1))
          } .otherwise {
            sda_reg := Bool(true)
          }
        }
      }
      
      when (state_done) {
        state := s_wait
      }
    }
    is (s_read) {
      clock_counter.io.en := Bool(true)

      when(clock_counter.io.top & (bit_pos <= UInt(8))) {
        scl_reg := ~scl_reg
      }
      
      when (sample & (bit_pos <= UInt(7))) {
        data_vec(bit_pos) := sda_in_reg
      }

      when (state_done) {
        state := s_wait
        sda_reg := Bool(true)
        scl_reg := Bool(false)
      }
    }
    is (s_stop) {
      clock_counter.io.en := Bool(true)
      
      // Set SCL high, then cause rising edge on SDA
      when (count_edge(0)) {
        scl_reg := Bool(true)
      } 
      when(count_edge(1)) {
        sda_reg := Bool(true)
      }
      
      when (state_done) {
        state := s_idle
      }
    }
  }
}

class I2CMasterTests(c: I2CMaster) extends Tester(c) {
  // Set divider to fastest clock
  poke(c.io.user.clock_div_in, 1)
  step(1)

  // Start condition
  poke(c.io.user.ctrl.start, 1)
  step(1)
  poke(c.io.user.ctrl.start, 0)
  for (i <- 0 until 4) {
    step(256)
  }
  expect(c.io.bus.scl.out, 0)
  expect(c.io.bus.sda.out, 0)

  // Write 0x42, expect ACK since SDA in stays low
  poke(c.io.user.ctrl.write,1)
  poke(c.io.user.data_in, 0x42)
  step(1)
  poke(c.io.user.ctrl.write,0)
  for(i <- 0 until 20) {
    step(256)
  }
  expect(c.io.user.ack, 1)

  // Read, expect 0xFF and NACK after setting sda to 1
  poke(c.io.user.ctrl.read,1)
  poke(c.io.bus.sda.in,1)
  step(1)
  poke(c.io.user.ctrl.read,0)
  for(i <- 0 until 20) {
    step(256)
  }
  expect(c.io.user.data_out, 0xFF)
  expect(c.io.user.ack, 0)

  // Stop condition, expect bus to be idle
  poke(c.io.user.ctrl.stop,1)
  step(1)
  poke(c.io.user.ctrl.stop,0)
  for(i <- 0 until 4) {
    step(256)
  }
  expect(c.io.bus.scl.out, 1)
  expect(c.io.bus.sda.out, 1)
}
