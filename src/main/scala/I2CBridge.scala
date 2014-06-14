package I2C

import Chisel._

class I2CBridge extends Module{
  val io = new Bundle {
    val host_in = Decoupled(UInt(width=8)).flip
    val host_out = Decoupled(Uint(width=8))

    val i2c = new I2CMasterIO().flip
  }
  
  val i2c_ready = Bool()
  i2c_ready := ~(i2c.ctrl_out.start | i2c.ctrl_out.stop | 
    i2c.ctrl_out.write | io.ctrl_out.read)
  
  // Default Values
  io.i2c.ctrl_in.start := Bool(false)
  io.i2c.ctrl_in.stop := Bool(false)
  io.i2c.ctrl_in.write := Bool(false)
  io.i2c.ctrl_in.read := Bool(false)

  io.host_in.ready := Bool(false)
  io.host_out.valid := Bool(false)
  io.host_out.bits := UInt(0)
  
  // Define escape values
  val e_ESC = UInt(0xFF,width=8)
  val e_SET_DIV :: e_GET_DIV :: e_SET_STAT :: e_GET_STAT :: Nil = Enum(Uint(width=8),7)


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
  i2c_stat := (UInt(0,width=2) ## io.i2c.active ## io.i2c.ack ## 
    io.i2c.ctrl_out.read ## io.i2c.ctrl_out.write ## 
    io.i2c.ctrl_out.stop ## io.i2c.ctrl_out.start)

  // Accumulate results from I2C and serialize to host out
  val read_buf = Vec.fill(3) {Reg(init=UInt(0,width=8))}
  val read_ptr = Reg(init = UInt(3,width=2))

  // Load read buffer so tokens are sent, starting at 0th element
  def load_read_buf(tokens: List[UInt]) = {
    read_ptr := UInt(tokens.length)
    for (i <- 0 until tokens.length) {
      read_buf(tokens.length-1-i) := tokens(i)
    }
  }

  io.host_out.valid := read_ptr != UInt(3)
  when (io.host_out.valid) {
    io.host_out.bits := read_buf(read_ptr)
  }
  
  when (io.host_out.fire()) {
    read_ptr := read_ptr - UInt(1)
  }

  // Detect when I2C read and write events have completed
  def falling_edge(n: Bool) = {
    ~n & Reg(next = n)
  }
  
  val i2c_write_done = Bool()
  i2c_write_done := falling_edge(io.i2c.ctrl_out.write)

  val i2c_read_done = Bool()
  i2c_read_done := falling_edge(io.i2c.ctrl_out.read)

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
        io.i2c.data_in := write_buf(0)
        io.i2c.ctrl_in.write := Bool(true)
      }
      is (UInt(1)) {
        io.i2c.data_in := e_ESC
        io.i2c.ctrl_in.write := Bool(true)
      }
      is (UInt(2)) {
        switch (write_buf(1)) {
          is (e_SET_DIV) {
            io.i2c.clock_div_in := write_buf(2)
          }
          is (e_SET_STAT) {
            io.i2c.ctrl_in.read := write_buf(2)(3)
            io.i2c.ctrl_in.write := write_buf(2)(2)
            io.i2c.ctrl_in.stop := write_buf(2)(1)
            io.i2c.ctrl_in.start := write_buf(2)(0)
          }
          is (e_GET_DIV) {
            write_ptr := Mux(read_buf_ready, UInt(3), write_ptr)
            when (read_buf_ready) {
              load_read_buf(List(e_ESC, e_GET_DIV, io.i2c.clock_div_out))
            }
          }
          is (e_GET_STAT) {
            write_ptr := Mux(read_buf_ready, UInt(3), write_ptr)
            when (read_buf_ready) {
              load_read_buf(List(e_ESC, e_GET_STAT, i2c_stat)
            }
          }
        }
      }
    }
  }

  val last_ack_token = Mux(io.i2c.ctrl_out.ack, e_ACK, e_NACK) 
  
  when (i2c_write_done) {
    load_read_buf(List(e_ESC, last_ack_token))
  }

  when (i2c_read_done) {
    load_read_buf(List(io.i2c.data_out, e_ESC, last_ack_token))
  }
}
