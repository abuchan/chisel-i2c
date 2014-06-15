package I2C

import Chisel._

class I2CBridgeIO extends Bundle{
  val host_in = Decoupled(UInt(width=8)).flip
  val host_out = Decoupled(UInt(width=8))

  val user = new I2CMasterUserIO().flip
}

class I2CBridge extends Module{
  val io = new I2CBridgeIO()
  
  val i2c_ready = Bool()
  i2c_ready := ~(io.user.stat.start | io.user.stat.stop | 
    io.user.stat.write | io.user.stat.read)
  
  // Default Values
  io.user.ctrl.start := Bool(false)
  io.user.ctrl.stop := Bool(false)
  io.user.ctrl.write := Bool(false)
  io.user.ctrl.read := Bool(false)

  io.user.clock_div_in := UInt(0,width=8)
  io.user.data_in := UInt(0,width=8)

  io.host_in.ready := Bool(false)
  io.host_out.valid := Bool(false)
  io.host_out.bits := UInt(0)
  
  // Define escape values
  val e_ESC = UInt(0xFF,width=8)
  val e_SET_DIV :: e_GET_DIV :: e_SET_STAT :: e_GET_STAT :: e_ACK :: e_NACK :: Nil = Enum(UInt(width=8),6)


  // Write buffer accumulates tokens from host in, and submits controls to I2C
  val write_buf = Vec.fill(3) {Reg(init=UInt(0,width=8))}
  val write_ptr = Reg(init = UInt(3,width=2))

  // write_buf_valid is true when a complete sequence of tokens has been read
  val write_buf_valid = Bool()
  write_buf_valid := Bool(false)
  switch(write_ptr) {
    is (UInt(0)) {
      write_buf_valid := write_buf(0) != e_ESC
    }
    is (UInt(1)) {
      write_buf_valid := write_buf(1) === e_ESC
    }
    is (UInt(2)) {
      write_buf_valid := Bool(true)
    }
  }
  
  io.host_in.ready := ~write_buf_valid

  // Bundle I2C status values
  val i2c_stat = UInt(width=8)
  i2c_stat := (UInt(0,width=2) ## io.user.active ## io.user.ack ## 
    io.user.stat.read ## io.user.stat.write ## 
    io.user.stat.stop ## io.user.stat.start)

  // Accumulate results from I2C and serialize to host out
  val n_read_buf = 4
  val read_buf = Vec.fill(n_read_buf) {Reg(init=UInt(0,width=8))}
  val read_ptr = Reg(init = UInt(n_read_buf,width=4))

  // Load read buffer so tokens are sent, starting at 0th element of tokens
  def load_read_buf(tokens: List[UInt]) = {
    read_ptr := UInt(tokens.length-1)
    for (i <- 0 until tokens.length) {
      read_buf(tokens.length-1-i) := tokens(i)
    }
  }

  io.host_out.valid := read_ptr != UInt(n_read_buf)
  when (io.host_out.valid) {
    io.host_out.bits := read_buf(read_ptr)
  }
  
  when (io.host_out.fire()) {
    when (read_ptr === UInt(0)) {
      read_ptr := UInt(n_read_buf)
    } .otherwise {
      read_ptr := read_ptr - UInt(1)
    }
  }

  // Detect when I2C read and write events have completed
  def falling_edge(n: Bool) = {
    ~n & Reg(next = n)
  }
  
  val i2c_write_done = Bool()
  i2c_write_done := falling_edge(io.user.stat.write)

  val i2c_read_done = Bool()
  i2c_read_done := falling_edge(io.user.stat.read)

  // True when data can be put in the outgoing read buffer
  val read_buf_ready = Bool()
  read_buf_ready := ~io.host_out.valid & ~i2c_write_done & ~i2c_read_done

  // Logic for dealing with host in tokens
  when (io.host_in.fire()) {
    write_buf(write_ptr + UInt(1)) := io.host_in.bits
    write_ptr := write_ptr + UInt(1)
  }
  .elsewhen (write_buf_valid & i2c_ready) {
    write_ptr := UInt(3)
    switch (write_ptr) {
      is (UInt(0)) {
        io.user.data_in := write_buf(0)
        io.user.ctrl.write := Bool(true)
      }
      is (UInt(1)) {
        io.user.data_in := e_ESC
        io.user.ctrl.write := Bool(true)
      }
      is (UInt(2)) {
        switch (write_buf(1)) {
          is (e_SET_DIV) {
            io.user.clock_div_in := write_buf(2)
          }
          is (e_SET_STAT) {
            io.user.ctrl.read := write_buf(2)(3)
            io.user.ctrl.write := write_buf(2)(2)
            io.user.ctrl.stop := write_buf(2)(1)
            io.user.ctrl.start := write_buf(2)(0)
          }
          is (e_GET_DIV) {
            write_ptr := Mux(read_buf_ready, UInt(3), write_ptr)
            when (read_buf_ready) {
              load_read_buf(List(e_ESC, e_GET_DIV, io.user.clock_div_out))
            }
          }
          is (e_GET_STAT) {
            write_ptr := Mux(read_buf_ready, UInt(3), write_ptr)
            when (read_buf_ready) {
              load_read_buf(List(e_ESC, e_GET_STAT, i2c_stat))
            }
          }
        }
      }
    }
  }

  val last_ack_token = Mux(io.user.ack, e_ACK, e_NACK) 
  
  when (i2c_write_done) {
    load_read_buf(List(e_ESC, last_ack_token))
  }

  when (i2c_read_done) {
    when (io.user.data_out === e_ESC) { // Need to escape if read 0xFF
      load_read_buf(List(e_ESC, e_ESC, e_ESC, last_ack_token))
    } .otherwise {
      load_read_buf(List(io.user.data_out, e_ESC, last_ack_token))
    }
  }
}
