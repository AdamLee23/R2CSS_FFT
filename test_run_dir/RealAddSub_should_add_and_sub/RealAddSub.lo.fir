circuit RealAddSub :
  module RealAddSub :
    input clock : Clock
    input reset : UInt<1>
    input io_a : SInt<32>
    input io_b : SInt<32>
    output io_A : SInt<32>
    output io_B : SInt<32>

    node _io_A_T = add(io_a, io_b) @[SDC_SDF_combFFT.scala 125:16]
    node _io_A_T_1 = tail(_io_A_T, 1) @[SDC_SDF_combFFT.scala 125:16]
    node _io_A_T_2 = asSInt(_io_A_T_1) @[SDC_SDF_combFFT.scala 125:16]
    node _io_B_T = sub(io_a, io_b) @[SDC_SDF_combFFT.scala 126:16]
    node _io_B_T_1 = tail(_io_B_T, 1) @[SDC_SDF_combFFT.scala 126:16]
    node _io_B_T_2 = asSInt(_io_B_T_1) @[SDC_SDF_combFFT.scala 126:16]
    io_A <= _io_A_T_2 @[SDC_SDF_combFFT.scala 125:8]
    io_B <= _io_B_T_2 @[SDC_SDF_combFFT.scala 126:8]
