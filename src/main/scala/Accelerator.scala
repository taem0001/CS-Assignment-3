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

  val idle :: read :: write :: done :: black :: neighbour :: up :: down :: left :: right = Enum(10)
  val state = RegInit(idle)
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val addressReg = RegInit(0.U(32.W))
  val dataReg = RegInit(0.U(32.W))

  switch(state) {
    is(idle) {
      when(io.start) {
        state := read
      }
    }
    is(read) {
      io.address := addressReg
      io.dataWrite := 0.U
      io.writeEnable := false.B
      when(x === 0.U || y === 0.U || x === 19.U || y === 19.U || io.dataRead === 0.U) {
        state := black
      }.elsewhen(!(x === 0.U || y === 0.U || x === 19.U || y === 19.U) && io.dataRead === 255.U) {
        state := neighbour
      }
    }
    is(black) {
      dataReg := 0.U
      io.dataWrite := dataReg
      state := write
    }
    is(write) {
      when(addressReg <= 399.U) {
        io.address := addressReg + 400.U
        when (y < 19.U) {
          y := y + 1.U
        } .otherwise {
          x := x + 1.U
          y := 0.U
        }
        addressReg := x + 20.U * y
        io.writeEnable := true.B
        state := read
      }
      state := done
    }
    
  }
}
