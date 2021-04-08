circuit Switch :
  module Switch :
    input clock : Clock
    input reset : UInt<1>
    input io_s : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _GEN_0 = mux(io_s, io_in1, io_in2) @[SDC_SDF_combFFT.scala 148:15 SDC_SDF_combFFT.scala 149:13 SDC_SDF_combFFT.scala 152:13]
    node _GEN_1 = mux(io_s, io_in2, io_in1) @[SDC_SDF_combFFT.scala 148:15 SDC_SDF_combFFT.scala 150:13 SDC_SDF_combFFT.scala 153:13]
    io_out1 <= _GEN_0
    io_out2 <= _GEN_1
