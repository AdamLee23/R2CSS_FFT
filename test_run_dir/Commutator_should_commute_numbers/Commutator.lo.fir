circuit Commutator :
  module Commutator :
    input clock : Clock
    input reset : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    input io_s : UInt<1>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    reg wire1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r) @[Reg.scala 15:16]
    node _GEN_0 = mux(UInt<1>("h1"), io_in2, wire1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_1) @[Reg.scala 15:16]
    node _GEN_1 = mux(UInt<1>("h1"), wire1_r, wire1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1 = wire1_r_1 @[SDC_SDF_combFFT.scala 108:19 SDC_SDF_combFFT.scala 110:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 111:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 109:19 SDC_SDF_combFFT.scala 111:9]
    node _GEN_2 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_1) @[Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), io_out1_r, io_out1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 113:17]
    io_out1 <= io_out1_r_1 @[SDC_SDF_combFFT.scala 112:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 113:11]
    wire1_r <= _GEN_0
    wire1_r_1 <= _GEN_1
    io_out1_r <= _GEN_2
    io_out1_r_1 <= _GEN_3
