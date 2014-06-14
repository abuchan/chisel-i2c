package I2C

import Chisel._

class I2CMasterCtrlIO extends Bundle{
  val start = Bool(INPUT)
  val stop = Bool(INPUT)
  val write = Bool(INPUT)
  val read = Bool(INPUT)
}

class I2CMasterIO extends Bundle{
  val ctrl_in = new I2CMasterCtrlIO()  
  val ctrl_out = new I2CMasterCtrlIO().flip
  
  val active = Bool(OUTPUT)
  val ack = Bool(OUTPUT)
  
  val clock_div_in = UInt(INPUT, width=8)
  val clock_div_out = UInt(OUTPUT, width=8)

  val data_in = UInt(INPUT, width=8)
  val data_out = UInt(OUTPUT, width=8)

  val scl_in = Bool(INPUT)
  val scl_out = Bool(OUTPUT)
  val sda_in = Bool(INPUT)
  val sda_out = Bool(OUTPUT)
}

class I2CMaster extends Module {
  val io = new I2CMasterIO()
  
  // Reg SCL and SDA to avoid glitches
  val scl_reg = Reg(init = Bool(true))
  val sda_reg = Reg(init = Bool(true))
  io.scl_out := scl_reg
  io.sda_out := sda_reg
  
  // Sample input lines
  val scl_in_reg = Reg(next = io.scl_in)
  val sda_in_reg = Reg(next = io.sda_in)

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
  io.clock_div_out := clock_max

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

  io.active := state != s_idle
  io.ctrl_out.start := state === s_start
  io.ctrl_out.stop := state === s_stop
  io.ctrl_out.write := state === s_write
  io.ctrl_out.read := state === s_read

  val data_vec = Vec.fill(8) {Reg(init=UInt(0,width=1))}
  io.data_out := data_vec.reduceRight[UInt](Cat(_,_))

  val ack_reg = Reg(init = Bool(false))
  io.ack := ack_reg
  
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

      unless (io.clock_div_in === UInt(0)) {
        clock_div := io.clock_div_in
      }

      when (io.ctrl_in.start) {
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

      when (io.ctrl_in.start) {
        state := s_start
        bit_max := UInt(2)
        sda_reg := Bool(true)
        scl_reg := Bool(true)
      }
      .elsewhen (io.ctrl_in.write) {
        state := s_write
        bit_max := UInt(18)
        // Latch in data, MSB is sent first
        for (i <- 0 until 8) {
          data_vec(i) := io.data_in(7-i)
        }
        sda_reg := io.data_in(7)
        scl_reg := Bool(false)
      }
      .elsewhen (io.ctrl_in.read) {
        state := s_read
        bit_max := UInt(18)
        sda_reg := Bool(true)
        scl_reg := Bool(false)
      }
      .elsewhen (io.ctrl_in.stop) {
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
        scl_reg := ~scl_reg

        when (scl_reg) {
          when (bit_pos < UInt(7)) {
            sda_reg := data_vec(bit_pos+UInt(1))
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

      when(clock_counter.io.top) {
        scl_reg := ~scl_reg
      }
      
      when (sample && (bit_pos <= UInt(7))) {
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
  poke(c.io.clock_div_in, 1)
  step(1)
  poke(c.io.ctrl_in.start, 1)
  step(1)
  poke(c.io.ctrl_in.start, 0)
  for (i <- 0 until 4) {
    step(256)
  }
  
  poke(c.io.ctrl_in.write,1)
  poke(c.io.data_in, 0x42)
  step(1)
  poke(c.io.ctrl_in.write,0)
  for(i <- 0 until 20) {
    step(256)
  }

  poke(c.io.ctrl_in.read,1)
  poke(c.io.sda_in,1)
  step(1)
  poke(c.io.ctrl_in.read,0)
  for(i <- 0 until 20) {
    step(256)
  }

  poke(c.io.ctrl_in.stop,1)
  step(1)
  poke(c.io.ctrl_in.stop,0)
  for(i <- 0 until 4) {
    step(256)
  }
}
