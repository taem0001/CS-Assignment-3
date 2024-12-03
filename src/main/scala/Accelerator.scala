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

  val idle :: read :: write :: increment :: done :: up :: down :: left :: right :: checkUp :: checkDown :: checkLeft :: checkRight :: Nil = Enum(13)
  val state = RegInit(idle)
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))
  val dataReadReg = RegNext(io.dataRead)
  val dataWriteReg = RegInit(0.U(32.W))

  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := dataWriteReg
  io.done := false.B

  switch(state) {
    is(idle) {
      when(io.start) {
        x := 0.U
        y := 0.U
        io.address := 0.U
        state := read
      }
    }
    is(read) {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U || dataReadReg === 0.U) {
        dataWriteReg := 0.U
        state := write
      }.otherwise {
        state := up
      }
    }
    is(write) {
      io.dataWrite := dataWriteReg
      io.address := (x + 20.U * y) + 400.U
      io.writeEnable := true.B
      state := increment
    }
    is(increment) {
      when(y < 19.U) {
        y := y + 1.U
      }.otherwise {
        x := x + 1.U
        y := 0.U
      }
      io.address := x + 20.U * y

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
    is(up) {
      io.address := x + 20.U * (y - 1.U)
      state := checkUp
    }
    is(checkUp) {
      when(dataReadReg === 255.U) {
        state := down
      }.otherwise {
        dataWriteReg := 0.U
        state := write
      }
    }
    is(down) {
      io.address := x + 20.U * (y + 1.U)
      state := checkDown
    }
    is(checkDown) {
      when(dataReadReg === 255.U) {
        state := left
      }.otherwise {
        dataWriteReg := 0.U
        state := write
      }
    }
    is(left) {
      io.address := (x - 1.U) + 20.U * y
      state := checkLeft
    }
    is(checkLeft) {
      when(dataReadReg === 255.U) {
        state := right
      }.otherwise {
        dataWriteReg := 0.U
        state := write
      }
    }
    is(right) {
      io.address := (x + 1.U) + 20.U * y
      state := checkRight
    }
    is(checkRight) {
      when(dataReadReg === 255.U) {
        dataWriteReg := 255.U
        state := write
      }.otherwise {
        dataWriteReg := 0.U
        state := write
      }
    }
  }
}
