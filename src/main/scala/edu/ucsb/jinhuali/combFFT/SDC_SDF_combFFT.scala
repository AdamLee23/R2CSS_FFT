package edu.ucsb.jinhuali.combFFT

import chisel3._
import chisel3.experimental._
import chisel3.util._

import scala.math._


/** some configuration */
trait HasConfig {
  val dataWidth = 32  // fix point number data width
  val binaryPoint = 16  // binary point
  val FFTLength = 128 // FFT length
}

/** complex number class */
class MyComplex extends Bundle with HasConfig {
  val re = FixedPoint(dataWidth.W, binaryPoint.BP)
  val im = FixedPoint(dataWidth.W, binaryPoint.BP)
}

/** complex number operation IO */
class ComplexOpIO extends Bundle {
  val op1 = Input(new MyComplex)
  val op2 = Input(new MyComplex)
  val res = Output(new MyComplex)
}

/** complex add module */
class ComplexAdd extends Module {
  val io = IO(new ComplexOpIO)
  io.res.re := io.op1.re + io.op2.re
  io.res.im := io.op1.im + io.op2.im
}

/** complex add companion object */
object ComplexAdd {
  def apply(op1: MyComplex, op2: MyComplex): MyComplex = {
    val inst = Module(new ComplexAdd)
    inst.io.op1 := op1
    inst.io.op2 := op2
    inst.io.res
  }
}

/** complex subtract module */
class ComplexSub extends Module {
  val io = IO(new ComplexOpIO)
  io.res.re := io.op1.re - io.op2.re
  io.res.im := io.op1.im - io.op2.im
}

/** complex subtract companion object */
object ComplexSub {
  def apply(op1: MyComplex, op2: MyComplex): MyComplex = {
    val inst = Module(new ComplexSub)
    inst.io.op1 := op1
    inst.io.op2 := op2
    inst.io.res
  }
}

/** data commutator
 *  s = 0: through; 1: switch
 */
class Commutator(dim: Int) extends Module with HasConfig {
  val io = IO(new Bundle() {
    val in1 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val in2 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val s = Input(Bool())
    val out1 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
    val out2 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
  })
  val wire1 = Wire(FixedPoint(dataWidth.W, binaryPoint.BP))
  val wire2 = Wire(FixedPoint(dataWidth.W, binaryPoint.BP))
  wire1 := ShiftRegister(io.in2, dim)
  wire2 := Mux(io.s, wire1, io.in1)
  io.out1 := ShiftRegister(wire2, dim)
  io.out2 := Mux(io.s, io.in1, wire1)
}

/** data commutator companion object */
object Commutator {
  def apply(input1: FixedPoint, input2: FixedPoint, select: Bool, dim: Int): (FixedPoint, FixedPoint) = {
    val inst = Module(new Commutator(dim))
    inst.io.in1 := input1
    inst.io.in2 := input2
    inst.io.s := select
    (inst.io.out1, inst.io.out2)
  }
}

/** SDF stage
 *  ctrlSub = 0: out := wire2 - in; 1: out := wire2 to prevent invalid input data
 *  from corrupting summation data saved in shift register
 *  ctrlAdd = 0: wire1 := wire2 + in; 1: wire1 := wire2 to prevent invalid input data
 *  from corrupting zeros saved in shift register after shift register reset
 */
class SDFUnit extends Module with HasConfig {
  val io = IO(new Bundle() {
    val in = Input(new MyComplex)
    val ctrlSub = Input(Bool())
    val ctrlAdd = Input(Bool())
    val out = Output(new MyComplex)
    val altRst = Input(Bool())
  })
  val wire1 = Wire(new MyComplex)
  val wire2 = Wire(new MyComplex)
  chisel3.withReset(io.altRst) {
    wire1 := Mux(io.ctrlAdd, wire2, ComplexAdd(wire2, io.in))
    io.out := Mux(io.ctrlSub, wire2, ComplexSub(wire2, io.in))
    wire2 := ShiftRegister(wire1, FFTLength / 2, 0.U((2*dataWidth).W).asTypeOf(new MyComplex), true.B)
  }
}

/** SDF companion unit */
object SDFUnit {
  def apply(input: MyComplex, controlSub: Bool, controlAdd: Bool, alternateReset: Bool): MyComplex = {
    val inst = Module(new SDFUnit)
    inst.io.in := input
    inst.io.ctrlSub := controlSub
    inst.io.ctrlAdd := controlAdd
    inst.io.altRst := alternateReset
    inst.io.out
  }
}

/** real number add/sub unit */
class RealAddSub extends Module with HasConfig {
  val io = IO(new Bundle() {
    val a = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val b = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val A = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
    val B = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
  })
  io.A := io.a + io.b
  io.B := io.a - io.b
}

/** RealAddSub companion object */
object RealAddSub {
  def apply(in1: FixedPoint, in2: FixedPoint): (FixedPoint, FixedPoint) = {
    val inst = Module(new RealAddSub)
    inst.io.a := in1
    inst.io.b := in2
    (inst.io.A, inst.io.B)
  }
}

/** switch
 *  s = 0: through; 1: switch
 */
class Switch extends Module with HasConfig {
  val io = IO(new Bundle() {
    val s = Input(Bool())
    val in1 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val in2 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val out1 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
    val out2 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
  })
  when (io.s) {
    io.out1 := io.in2
    io.out2 := io.in1
  } otherwise {
    io.out1 := io.in1
    io.out2 := io.in2
  }
}

/** switch companion object */
object Switch {
  def apply(in1: FixedPoint, in2: FixedPoint, select: Bool) = {
    val inst = Module(new Switch)
    inst.io.in1 := in1
    inst.io.in2 := in2
    inst.io.s := select
    (inst.io.out1, inst.io.out2)
  }
}

/** real multiplier */
class RealBranchMul extends Module with HasConfig {
  val io = IO(new Bundle() {
    val dataIn = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val w_re_in = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val w_im_in = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val out1 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
    val out2 = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
  })
  io.out1 := io.w_re_in * io.dataIn
  io.out2 := io.w_im_in * io.dataIn
}

/** RealBranchMult companion object */
object RealBranchMul {
  def apply(dataInput: FixedPoint, w_re_input: FixedPoint, w_im_input: FixedPoint): (FixedPoint, FixedPoint) = {
    val inst = Module(new RealBranchMul)
    inst.io.dataIn := dataInput
    inst.io.w_re_in := w_re_input
    inst.io.w_im_in := w_im_input
    (inst.io.out1, inst.io.out2)
  }
}

/** real adder/subtractor controlled by select signal
 *  s = 0: adder; 1: subtractor
 */
class RealAdder extends Module with HasConfig {
  val io = IO(new Bundle() {
    val in1 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val in2 = Input(FixedPoint(dataWidth.W, binaryPoint.BP))
    val s = Input(Bool())
    val out = Output(FixedPoint(dataWidth.W, binaryPoint.BP))
  })
  when (io.s) {
    io.out := io.in1 - io.in2
  } otherwise {
    io.out := io.in1 + io.in2
  }
}

/** RealAdder companion object */
object RealAdder {
  def apply(dataInput1: FixedPoint, dataInput2: FixedPoint, select: Bool): FixedPoint = {
    val inst = Module(new RealAdder)
    inst.io.in1 := dataInput1
    inst.io.in2 := dataInput2
    inst.io.s := select
    inst.io.out
  }
}

/** bit reverser for reordering data to normal order
 *  basically a RAM with specified size
 *  first half data: reverse least log2(FFT)-1 bits and add 8 to get correct index
 *  last half data: reverse least log2(FFT)-1 bits to get correct index
 */
class Reorder extends Module with HasConfig {
  val io = IO(new Bundle() {
    val in = Input(new MyComplex)
    val out = Output(new MyComplex)
    val inValid = Input(Bool())
    val outValid = Output(Bool())
  })
  val ram = Mem(FFTLength, new MyComplex)
  val width = log2Ceil(FFTLength)
  val inCounter = RegInit(0.U(width.W))
  val inCounterCut = RegInit(0.U((width-1).W))
  val outCounter = RegInit(0.U(width.W))
  val index0 = Reverse(inCounterCut)
  val index1 = Reverse(inCounterCut) + (FFTLength / 2).U
  when (io.inValid || inCounter =/= 0.U) {
    when (inCounter < (FFTLength / 2).U) {
      ram.write(index1, io.in)
    } otherwise {
      ram.write(index0, io.in)
    }
    inCounter := inCounter + 1.U
    inCounterCut := inCounterCut + 1.U
  }
  io.outValid := (RegNext(inCounter) === (FFTLength - 1).U) || (outCounter =/= 0.U)
  when (io.outValid) {
    outCounter := outCounter + 1.U
  }
  io.out := ram.read(outCounter)
}

/** combined SDC-SDF FFT */
class CombFFT extends Module with HasConfig {
  val io = IO(new Bundle() {
    val dataIn = Input(new MyComplex)
    val inValid = Input(Bool())
    val dataOut = Output(new MyComplex)
    val outValid = Output(Bool())
    val busy = Output(Bool())
  })

  // construct twiddle factor ROM for FFT
  def W(index: UInt): (FixedPoint, FixedPoint) = {
    val times = (0 until (FFTLength/2)).map(i => 2*Pi*i/FFTLength)
    val re_s = VecInit(times.map(i => FixedPoint.fromDouble(cos(i), dataWidth.W, binaryPoint.BP)))
    val im_s = VecInit(times.map(i => FixedPoint.fromDouble(-sin(i), dataWidth.W, binaryPoint.BP)))
    (re_s(index), im_s(index))
  }

  // twiddle factor index ROM for SDC stage s_i
  def WIndexVec(s_i: Int) = {
    val v = for {i <- 0 until FFTLength / 2 by pow(2, s_i).toInt} yield i
    val even = for {i <- v.indices if i % 2 == 0} yield i
    val odd = for {i <- v.indices if i % 2 != 0} yield i
    var repTwice = Vector[Int]()
    for (i <- even ++ odd) repTwice = repTwice ++ Vector(i, i)
    val raw = for {i <- repTwice} yield v(i)
    assert(raw.length % 2 ==0)
    val rawHead = raw.splitAt(raw.length / 2)._1
    val rawTail = raw.splitAt(raw.length / 2)._2
    val wIndexVec = (0 until pow(2, s_i).toInt).foldLeft(Vector[Int]()) { (z, i) => z ++ rawHead } ++
      (0 until pow(2, s_i).toInt).foldLeft(Vector[Int]()) { (z, i) => z ++ rawTail }
    VecInit(wIndexVec.map(i => i.U))
  }

  val stageNum = log2Ceil(FFTLength) - 1
  val dCount = RegInit(0.U(log2Ceil(3*FFTLength+stageNum).W))
  val busy = dCount =/= 0.U
  when(io.inValid || busy){
    dCount := Mux(dCount === (3.5*FFTLength+stageNum-1).toInt.U, 0.U, dCount+1.U)
  }
  io.busy := busy
  // wires for interfaces among stages
  val stageIntf1, stageIntf2 = VecInit(Seq.fill(stageNum+1)(FixedPoint.fromDouble(0.0, dataWidth.W, binaryPoint.BP)))
  // pre-stage
  val commutatorReturn = Commutator(io.dataIn.re, io.dataIn.im, dCount(0).asBool(), 1)
  stageIntf1(0) := commutatorReturn._1
  stageIntf2(0) := commutatorReturn._2
  // repetitive parts, i.e. SDC stages
  for (s_i <- 0 until stageNum) {
    val ws = s_i+1+FFTLength*(1-pow(2, -s_i-1)) // time i_th stage starts to require w
    val wsU = round(ws).toInt.U
    val we = ws + FFTLength // time i_th stage ends to require w (every stage needs FFTLength cycles of w provision)
    val wReturn = W(WIndexVec(s_i)(dCount-wsU))
    val wireWRe, wireWIm = Wire(FixedPoint())
    wireWRe := Mux(dCount >= wsU && dCount < round(we).toInt.U, wReturn._1, FixedPoint.fromDouble(0.0, dataWidth.W, binaryPoint.BP))
    wireWIm := Mux(dCount >= wsU && dCount < round(we).toInt.U, wReturn._2, FixedPoint.fromDouble(0.0, dataWidth.W, binaryPoint.BP))
    // SDC stage hardware
    // data commutator
    val DCDim = round(FFTLength/pow(2, s_i+1)).toInt
    val wireSelectDC = Wire(Bool())
    wireSelectDC := Mux(dCount >= wsU, !(dCount-wsU)(log2Ceil(DCDim)).asBool(), false.B)
    val dataCommutatorReturn = Commutator(stageIntf1(s_i), stageIntf2(s_i), wireSelectDC, DCDim)
    // real add/sub unit
    val realAddSubReturn = RealAddSub(dataCommutatorReturn._1, dataCommutatorReturn._2)
    // real multiplier
    val realMultReturn = RealBranchMul(realAddSubReturn._2, wireWRe, wireWIm)
    // common select signal for switch, data commutator of dim 1, and real adder
    val commonSelt = Wire(Bool())
    commonSelt := Mux((s_i%2==0).asBool(), !dCount(0).asBool(), dCount(0).asBool())
    // switch
    val switchReturn = Switch(realMultReturn._1, realMultReturn._2, commonSelt)
    // commutator of dimensionality 1
    val commDim1Return = Commutator(switchReturn._1, switchReturn._2, commonSelt, 1)
    // real adder
    stageIntf2(s_i+1) := RealAdder(commDim1Return._1, commDim1Return._2, commonSelt)
    stageIntf1(s_i+1) := RegNext(realAddSubReturn._1)
  }
  // post stage
  val postStgSelt = Mux((stageNum%2==0).asBool(), !dCount(0).asBool(), dCount(0).asBool())
  val postStageReturn = Commutator(stageIntf1(stageNum), stageIntf2(stageNum), postStgSelt, 1)
  // SDF stage
  val wireFpToComp = Wire(new MyComplex)
  wireFpToComp.re := postStageReturn._1
  wireFpToComp.im := postStageReturn._2
  // SDF control
  val wireSDFAdd, wireSDFSub, wireSDFRst = Wire(Bool())
  wireSDFAdd := Mux(dCount >= (FFTLength+stageNum).U && dCount < (2*FFTLength+stageNum).U, false.B, true.B) // add in operation condition
  wireSDFSub := Mux(dCount >= (1.5*FFTLength+stageNum).toInt.U && dCount < (2*FFTLength+stageNum).U, false.B, true.B) // sub in operation condition
  wireSDFRst := Mux(dCount === (2.5*FFTLength+stageNum-1).toInt.U, true.B, false.B) // reset shift register to zero for next FFT computation
  val sdfReturn = SDFUnit(wireFpToComp, wireSDFSub, wireSDFAdd, wireSDFRst)
  // Reorder
  val reorderModule = Module(new Reorder)
  reorderModule.io.in := sdfReturn
  reorderModule.io.inValid := RegNext(dCount) === (1.5*FFTLength+stageNum-1).toInt.U
  io.dataOut := reorderModule.io.out
  io.outValid := reorderModule.io.outValid
}
