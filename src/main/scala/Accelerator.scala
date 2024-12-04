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

  val idle :: read :: write :: done :: checkUp :: checkDown :: checkLeft :: checkRight :: Nil = Enum(8)
  val state = RegInit(idle)
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))
  val dataWriteReg = RegInit(0.U(32.W))
  val addressReg = RegInit(0.U(16.W))

  io.writeEnable := false.B
  io.dataWrite := dataWriteReg
  io.address := addressReg
  io.done := false.B

  switch(state) {
    is(idle) {
      when(io.start) {
        x := 0.U
        y := 0.U
        addressReg := 0.U
        state := read
      }
    }
    is(read) {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U || io.dataRead === 0.U) {
        dataWriteReg := 0.U
        addressReg := (x + 20.U * y) + 400.U
        state := write
      }.otherwise {
        addressReg := x + 20.U * (y - 1.U)
        state := checkUp
      }
    }
    is(write) {
      io.writeEnable := true.B
      when(y < 19.U) {
        y := y + 1.U
      }.otherwise {
        x := x + 1.U
        y := 0.U
      }
      addressReg := x + 20.U * y
      when(x < 20.U && y < 20.U) {
        state := read
      }.otherwise {
        state := done
      }
    }
    is(done) {
      io.done := true.B
      state := done
    }
    is(checkUp) {
      when(io.dataRead === 255.U) {
        addressReg := x + 20.U * (y + 1.U)
        state := checkDown
      }.otherwise {
        dataWriteReg := 0.U
        addressReg := (x + 20.U * y) + 400.U
        state := write
      }
    }
    is(checkDown) {
      when(io.dataRead === 255.U) {
        addressReg := (x - 1.U) + 20.U * y
        state := checkLeft
      }.otherwise {
        dataWriteReg := 0.U
        addressReg := (x + 20.U * y) + 400.U
        state := write
      }
    }
    is(checkLeft) {
      when(io.dataRead === 255.U) {
        addressReg := (x + 1.U) + 20.U * y
        state := checkRight
      }.otherwise {
        dataWriteReg := 0.U
        addressReg := (x + 20.U * y) + 400.U
        state := write
      }
    }
    is(checkRight) {
      addressReg := (x + 20.U * y) + 400.U
      when(io.dataRead === 255.U) {
        dataWriteReg := 255.U
        state := write
      }.otherwise {
        dataWriteReg := 0.U
        state := write
      }
    }
  }
}
