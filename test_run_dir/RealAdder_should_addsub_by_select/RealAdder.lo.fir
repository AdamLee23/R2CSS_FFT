circuit RealAdder :
  module RealAdder :
    input clock : Clock
    input reset : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    input io_s : UInt<1>
    output io_out : SInt<32>

    node _io_out_T = add(io_in1, io_in2) @[SDC_SDF_combFFT.scala 200:22]
    node _io_out_T_1 = tail(_io_out_T, 1) @[SDC_SDF_combFFT.scala 200:22]
    node _io_out_T_2 = asSInt(_io_out_T_1) @[SDC_SDF_combFFT.scala 200:22]
    node _io_out_T_3 = sub(io_in1, io_in2) @[SDC_SDF_combFFT.scala 202:22]
    node _io_out_T_4 = tail(_io_out_T_3, 1) @[SDC_SDF_combFFT.scala 202:22]
    node _io_out_T_5 = asSInt(_io_out_T_4) @[SDC_SDF_combFFT.scala 202:22]
    node _GEN_0 = mux(io_s, _io_out_T_2, _io_out_T_5) @[SDC_SDF_combFFT.scala 199:15 SDC_SDF_combFFT.scala 200:12 SDC_SDF_combFFT.scala 202:12]
    io_out <= _GEN_0
