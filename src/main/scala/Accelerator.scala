import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  val idle :: read :: write :: done :: black :: up :: down :: left :: right :: check :: Nil = Enum(10)
  val state = RegInit(idle)

  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))

  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := dataReg
  io.done := false.B

  switch(state) {
    is(idle) {
      when(io.start) {
        state := read
        addressReg := 0.U
        x := 0.U
        y := 0.U
      }
    }
    is(read) {
      io.address := addressReg

      when(x === 0.U || y === 0.U || x === 19.U || y === 19.U || io.dataRead === 0.U) {
        state := black
      }.otherwise {
        state := up
      }
    }
    is(write) {
      io.address := addressReg + 400.U
      io.writeEnable := true.B
      addressReg := x + 20.U * y
      when(y < 19.U) {
        y := y + 1.U
      }.otherwise {
        x := x + 1.U
        y := 0.U
      }
      when(addressReg < 399.U) {
        state := read
      }.otherwise {
        state := done
      }
    }
    is(done) {
      io.done := true.B
      state := done
    }
    is(black) {
      dataReg := 0.U
      state := write
    }
    is(up) {
      io.address := x + 20.U * (y - 1.U)

      when(io.dataRead === 255.U) {
        state := down
      }.otherwise {
        state := black
      }
    }
    is(down) {
      io.address := x + 20.U * (y + 1.U)

      when(io.dataRead === 255.U) {
        state := right
      }.otherwise {
        state := black
      }
    }
    is(right) {
      io.address := (x + 1.U) + 20.U * y

      when(io.dataRead === 255.U) {
        state := left
      }.otherwise {
        state := black
      }
    }
    is(left) {
      io.address := (x - 1.U) + 20.U * y

      when(io.dataRead === 255.U) {
        state := check
      }.otherwise {
        state := black
      }
    }
    is(check) {
      dataReg := 255.U
      state := write
    }
  }
}
