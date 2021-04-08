circuit CombFFT :
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
    node wire1 = wire1_r @[SDC_SDF_combFFT.scala 75:19 SDC_SDF_combFFT.scala 77:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 78:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 76:19 SDC_SDF_combFFT.scala 78:9]
    node _GEN_1 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 80:17]
    io_out1 <= io_out1_r @[SDC_SDF_combFFT.scala 79:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 80:11]
    wire1_r <= _GEN_0
    io_out1_r <= _GEN_1

  module Commutator_1 :
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
    reg wire1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_2) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), wire1_r_1, wire1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_3) @[Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), wire1_r_2, wire1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_4 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_4) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), wire1_r_3, wire1_r_4) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_5 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_5) @[Reg.scala 15:16]
    node _GEN_5 = mux(UInt<1>("h1"), wire1_r_4, wire1_r_5) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_6 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_6) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), wire1_r_5, wire1_r_6) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_7 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_7) @[Reg.scala 15:16]
    node _GEN_7 = mux(UInt<1>("h1"), wire1_r_6, wire1_r_7) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_8 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_8) @[Reg.scala 15:16]
    node _GEN_8 = mux(UInt<1>("h1"), wire1_r_7, wire1_r_8) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_9 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_9) @[Reg.scala 15:16]
    node _GEN_9 = mux(UInt<1>("h1"), wire1_r_8, wire1_r_9) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_10 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_10) @[Reg.scala 15:16]
    node _GEN_10 = mux(UInt<1>("h1"), wire1_r_9, wire1_r_10) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_11 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_11) @[Reg.scala 15:16]
    node _GEN_11 = mux(UInt<1>("h1"), wire1_r_10, wire1_r_11) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_12 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_12) @[Reg.scala 15:16]
    node _GEN_12 = mux(UInt<1>("h1"), wire1_r_11, wire1_r_12) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_13 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_13) @[Reg.scala 15:16]
    node _GEN_13 = mux(UInt<1>("h1"), wire1_r_12, wire1_r_13) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_14 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_14) @[Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), wire1_r_13, wire1_r_14) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_15 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_15) @[Reg.scala 15:16]
    node _GEN_15 = mux(UInt<1>("h1"), wire1_r_14, wire1_r_15) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1 = wire1_r_15 @[SDC_SDF_combFFT.scala 75:19 SDC_SDF_combFFT.scala 77:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 78:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 76:19 SDC_SDF_combFFT.scala 78:9]
    node _GEN_16 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_1) @[Reg.scala 15:16]
    node _GEN_17 = mux(UInt<1>("h1"), io_out1_r, io_out1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_2) @[Reg.scala 15:16]
    node _GEN_18 = mux(UInt<1>("h1"), io_out1_r_1, io_out1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_3) @[Reg.scala 15:16]
    node _GEN_19 = mux(UInt<1>("h1"), io_out1_r_2, io_out1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_4 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_4) @[Reg.scala 15:16]
    node _GEN_20 = mux(UInt<1>("h1"), io_out1_r_3, io_out1_r_4) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_5 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_5) @[Reg.scala 15:16]
    node _GEN_21 = mux(UInt<1>("h1"), io_out1_r_4, io_out1_r_5) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_6 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_6) @[Reg.scala 15:16]
    node _GEN_22 = mux(UInt<1>("h1"), io_out1_r_5, io_out1_r_6) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_7 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_7) @[Reg.scala 15:16]
    node _GEN_23 = mux(UInt<1>("h1"), io_out1_r_6, io_out1_r_7) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_8 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_8) @[Reg.scala 15:16]
    node _GEN_24 = mux(UInt<1>("h1"), io_out1_r_7, io_out1_r_8) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_9 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_9) @[Reg.scala 15:16]
    node _GEN_25 = mux(UInt<1>("h1"), io_out1_r_8, io_out1_r_9) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_10 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_10) @[Reg.scala 15:16]
    node _GEN_26 = mux(UInt<1>("h1"), io_out1_r_9, io_out1_r_10) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_11 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_11) @[Reg.scala 15:16]
    node _GEN_27 = mux(UInt<1>("h1"), io_out1_r_10, io_out1_r_11) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_12 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_12) @[Reg.scala 15:16]
    node _GEN_28 = mux(UInt<1>("h1"), io_out1_r_11, io_out1_r_12) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_13 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_13) @[Reg.scala 15:16]
    node _GEN_29 = mux(UInt<1>("h1"), io_out1_r_12, io_out1_r_13) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_14 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_14) @[Reg.scala 15:16]
    node _GEN_30 = mux(UInt<1>("h1"), io_out1_r_13, io_out1_r_14) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_15 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_15) @[Reg.scala 15:16]
    node _GEN_31 = mux(UInt<1>("h1"), io_out1_r_14, io_out1_r_15) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 80:17]
    io_out1 <= io_out1_r_15 @[SDC_SDF_combFFT.scala 79:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 80:11]
    wire1_r <= _GEN_0
    wire1_r_1 <= _GEN_1
    wire1_r_2 <= _GEN_2
    wire1_r_3 <= _GEN_3
    wire1_r_4 <= _GEN_4
    wire1_r_5 <= _GEN_5
    wire1_r_6 <= _GEN_6
    wire1_r_7 <= _GEN_7
    wire1_r_8 <= _GEN_8
    wire1_r_9 <= _GEN_9
    wire1_r_10 <= _GEN_10
    wire1_r_11 <= _GEN_11
    wire1_r_12 <= _GEN_12
    wire1_r_13 <= _GEN_13
    wire1_r_14 <= _GEN_14
    wire1_r_15 <= _GEN_15
    io_out1_r <= _GEN_16
    io_out1_r_1 <= _GEN_17
    io_out1_r_2 <= _GEN_18
    io_out1_r_3 <= _GEN_19
    io_out1_r_4 <= _GEN_20
    io_out1_r_5 <= _GEN_21
    io_out1_r_6 <= _GEN_22
    io_out1_r_7 <= _GEN_23
    io_out1_r_8 <= _GEN_24
    io_out1_r_9 <= _GEN_25
    io_out1_r_10 <= _GEN_26
    io_out1_r_11 <= _GEN_27
    io_out1_r_12 <= _GEN_28
    io_out1_r_13 <= _GEN_29
    io_out1_r_14 <= _GEN_30
    io_out1_r_15 <= _GEN_31

  module RealAddSub :
    input clock : Clock
    input reset : UInt<1>
    input io_a : SInt<32>
    input io_b : SInt<32>
    output io_A : SInt<32>
    output io_B : SInt<32>

    node _io_A_T = add(io_a, io_b) @[SDC_SDF_combFFT.scala 137:16]
    node _io_A_T_1 = tail(_io_A_T, 1) @[SDC_SDF_combFFT.scala 137:16]
    node _io_A_T_2 = asSInt(_io_A_T_1) @[SDC_SDF_combFFT.scala 137:16]
    node _io_B_T = sub(io_a, io_b) @[SDC_SDF_combFFT.scala 138:16]
    node _io_B_T_1 = tail(_io_B_T, 1) @[SDC_SDF_combFFT.scala 138:16]
    node _io_B_T_2 = asSInt(_io_B_T_1) @[SDC_SDF_combFFT.scala 138:16]
    io_A <= _io_A_T_2 @[SDC_SDF_combFFT.scala 137:8]
    io_B <= _io_B_T_2 @[SDC_SDF_combFFT.scala 138:8]

  module RealBranchMul :
    input clock : Clock
    input reset : UInt<1>
    input io_dataIn : SInt<32>
    input io_w_re_in : SInt<32>
    input io_w_im_in : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _io_out1_T = mul(io_w_re_in, io_dataIn) @[SDC_SDF_combFFT.scala 191:25]
    node _io_out2_T = mul(io_w_im_in, io_dataIn) @[SDC_SDF_combFFT.scala 192:25]
    io_out1 <= asSInt(bits(shr(_io_out1_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 191:11]
    io_out2 <= asSInt(bits(shr(_io_out2_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 192:11]

  module Switch :
    input clock : Clock
    input reset : UInt<1>
    input io_s : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _GEN_0 = mux(io_s, io_in2, io_in1) @[SDC_SDF_combFFT.scala 162:15 SDC_SDF_combFFT.scala 163:13 SDC_SDF_combFFT.scala 166:13]
    node _GEN_1 = mux(io_s, io_in1, io_in2) @[SDC_SDF_combFFT.scala 162:15 SDC_SDF_combFFT.scala 164:13 SDC_SDF_combFFT.scala 167:13]
    io_out1 <= _GEN_0
    io_out2 <= _GEN_1

  module RealAdder :
    input clock : Clock
    input reset : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    input io_s : UInt<1>
    output io_out : SInt<32>

    node _io_out_T = sub(io_in1, io_in2) @[SDC_SDF_combFFT.scala 217:22]
    node _io_out_T_1 = tail(_io_out_T, 1) @[SDC_SDF_combFFT.scala 217:22]
    node _io_out_T_2 = asSInt(_io_out_T_1) @[SDC_SDF_combFFT.scala 217:22]
    node _io_out_T_3 = add(io_in1, io_in2) @[SDC_SDF_combFFT.scala 219:22]
    node _io_out_T_4 = tail(_io_out_T_3, 1) @[SDC_SDF_combFFT.scala 219:22]
    node _io_out_T_5 = asSInt(_io_out_T_4) @[SDC_SDF_combFFT.scala 219:22]
    node _GEN_0 = mux(io_s, _io_out_T_2, _io_out_T_5) @[SDC_SDF_combFFT.scala 216:15 SDC_SDF_combFFT.scala 217:12 SDC_SDF_combFFT.scala 219:12]
    io_out <= _GEN_0

  module Commutator_3 :
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
    reg wire1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_2) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), wire1_r_1, wire1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_3) @[Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), wire1_r_2, wire1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_4 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_4) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), wire1_r_3, wire1_r_4) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_5 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_5) @[Reg.scala 15:16]
    node _GEN_5 = mux(UInt<1>("h1"), wire1_r_4, wire1_r_5) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_6 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_6) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), wire1_r_5, wire1_r_6) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_7 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_7) @[Reg.scala 15:16]
    node _GEN_7 = mux(UInt<1>("h1"), wire1_r_6, wire1_r_7) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1 = wire1_r_7 @[SDC_SDF_combFFT.scala 75:19 SDC_SDF_combFFT.scala 77:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 78:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 76:19 SDC_SDF_combFFT.scala 78:9]
    node _GEN_8 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_1) @[Reg.scala 15:16]
    node _GEN_9 = mux(UInt<1>("h1"), io_out1_r, io_out1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_2) @[Reg.scala 15:16]
    node _GEN_10 = mux(UInt<1>("h1"), io_out1_r_1, io_out1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_3) @[Reg.scala 15:16]
    node _GEN_11 = mux(UInt<1>("h1"), io_out1_r_2, io_out1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_4 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_4) @[Reg.scala 15:16]
    node _GEN_12 = mux(UInt<1>("h1"), io_out1_r_3, io_out1_r_4) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_5 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_5) @[Reg.scala 15:16]
    node _GEN_13 = mux(UInt<1>("h1"), io_out1_r_4, io_out1_r_5) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_6 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_6) @[Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), io_out1_r_5, io_out1_r_6) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_7 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_7) @[Reg.scala 15:16]
    node _GEN_15 = mux(UInt<1>("h1"), io_out1_r_6, io_out1_r_7) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 80:17]
    io_out1 <= io_out1_r_7 @[SDC_SDF_combFFT.scala 79:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 80:11]
    wire1_r <= _GEN_0
    wire1_r_1 <= _GEN_1
    wire1_r_2 <= _GEN_2
    wire1_r_3 <= _GEN_3
    wire1_r_4 <= _GEN_4
    wire1_r_5 <= _GEN_5
    wire1_r_6 <= _GEN_6
    wire1_r_7 <= _GEN_7
    io_out1_r <= _GEN_8
    io_out1_r_1 <= _GEN_9
    io_out1_r_2 <= _GEN_10
    io_out1_r_3 <= _GEN_11
    io_out1_r_4 <= _GEN_12
    io_out1_r_5 <= _GEN_13
    io_out1_r_6 <= _GEN_14
    io_out1_r_7 <= _GEN_15

  module Commutator_5 :
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
    reg wire1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_2) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), wire1_r_1, wire1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire1_r_3) @[Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), wire1_r_2, wire1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1 = wire1_r_3 @[SDC_SDF_combFFT.scala 75:19 SDC_SDF_combFFT.scala 77:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 78:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 76:19 SDC_SDF_combFFT.scala 78:9]
    node _GEN_4 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_1) @[Reg.scala 15:16]
    node _GEN_5 = mux(UInt<1>("h1"), io_out1_r, io_out1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_2) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), io_out1_r_1, io_out1_r_2) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_3) @[Reg.scala 15:16]
    node _GEN_7 = mux(UInt<1>("h1"), io_out1_r_2, io_out1_r_3) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 80:17]
    io_out1 <= io_out1_r_3 @[SDC_SDF_combFFT.scala 79:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 80:11]
    wire1_r <= _GEN_0
    wire1_r_1 <= _GEN_1
    wire1_r_2 <= _GEN_2
    wire1_r_3 <= _GEN_3
    io_out1_r <= _GEN_4
    io_out1_r_1 <= _GEN_5
    io_out1_r_2 <= _GEN_6
    io_out1_r_3 <= _GEN_7

  module Commutator_7 :
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
    node wire1 = wire1_r_1 @[SDC_SDF_combFFT.scala 75:19 SDC_SDF_combFFT.scala 77:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 78:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 76:19 SDC_SDF_combFFT.scala 78:9]
    node _GEN_2 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg io_out1_r_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r_1) @[Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), io_out1_r, io_out1_r_1) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 80:17]
    io_out1 <= io_out1_r_1 @[SDC_SDF_combFFT.scala 79:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 80:11]
    wire1_r <= _GEN_0
    wire1_r_1 <= _GEN_1
    io_out1_r <= _GEN_2
    io_out1_r_1 <= _GEN_3

  module ComplexAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = add(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 33:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 33:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 33:26]
    node _io_res_im_T = add(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 34:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 34:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 34:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 33:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 34:13]

  module ComplexSub :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = sub(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 50:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 50:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 50:26]
    node _io_res_im_T = sub(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 51:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 51:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 51:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 50:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 51:13]

  module SDFUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    input io_ctrlSub : UInt<1>
    input io_ctrlAdd : UInt<1>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>
    input io_altRst : UInt<1>

    inst wire1_inst of ComplexAdd @[SDC_SDF_combFFT.scala 40:22]
    inst io_out_inst of ComplexSub @[SDC_SDF_combFFT.scala 57:22]
    reg wire2_r_15_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_15_re) @[Reg.scala 27:20]
    node wire2_re = wire2_r_15_re @[SDC_SDF_combFFT.scala 109:19 SDC_SDF_combFFT.scala 113:11]
    node _wire1_T_re = mux(io_ctrlAdd, wire2_re, wire1_inst.io_res_re) @[SDC_SDF_combFFT.scala 111:17]
    reg wire2_r_15_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_15_im) @[Reg.scala 27:20]
    node wire2_im = wire2_r_15_im @[SDC_SDF_combFFT.scala 109:19 SDC_SDF_combFFT.scala 113:11]
    node _wire1_T_im = mux(io_ctrlAdd, wire2_im, wire1_inst.io_res_im) @[SDC_SDF_combFFT.scala 111:17]
    node _io_out_T_re = mux(io_ctrlSub, wire2_re, io_out_inst.io_res_re) @[SDC_SDF_combFFT.scala 112:18]
    node _io_out_T_im = mux(io_ctrlSub, wire2_im, io_out_inst.io_res_im) @[SDC_SDF_combFFT.scala 112:18]
    node _wire2_T = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 113:79]
    node _wire2_T_1 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 113:79]
    reg wire2_r_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_re) @[Reg.scala 27:20]
    reg wire2_r_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_im) @[Reg.scala 27:20]
    node wire1_im = _wire1_T_im @[SDC_SDF_combFFT.scala 108:19 SDC_SDF_combFFT.scala 111:11]
    node _GEN_0 = mux(UInt<1>("h1"), wire1_im, wire2_r_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node wire1_re = _wire1_T_re @[SDC_SDF_combFFT.scala 108:19 SDC_SDF_combFFT.scala 111:11]
    node _GEN_1 = mux(UInt<1>("h1"), wire1_re, wire2_r_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_1_re) @[Reg.scala 27:20]
    reg wire2_r_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_1_im) @[Reg.scala 27:20]
    node _GEN_2 = mux(UInt<1>("h1"), wire2_r_im, wire2_r_1_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_3 = mux(UInt<1>("h1"), wire2_r_re, wire2_r_1_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_2_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_2_re) @[Reg.scala 27:20]
    reg wire2_r_2_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_2_im) @[Reg.scala 27:20]
    node _GEN_4 = mux(UInt<1>("h1"), wire2_r_1_im, wire2_r_2_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_5 = mux(UInt<1>("h1"), wire2_r_1_re, wire2_r_2_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_3_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_3_re) @[Reg.scala 27:20]
    reg wire2_r_3_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_3_im) @[Reg.scala 27:20]
    node _GEN_6 = mux(UInt<1>("h1"), wire2_r_2_im, wire2_r_3_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_7 = mux(UInt<1>("h1"), wire2_r_2_re, wire2_r_3_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_4_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_4_re) @[Reg.scala 27:20]
    reg wire2_r_4_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_4_im) @[Reg.scala 27:20]
    node _GEN_8 = mux(UInt<1>("h1"), wire2_r_3_im, wire2_r_4_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_9 = mux(UInt<1>("h1"), wire2_r_3_re, wire2_r_4_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_5_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_5_re) @[Reg.scala 27:20]
    reg wire2_r_5_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_5_im) @[Reg.scala 27:20]
    node _GEN_10 = mux(UInt<1>("h1"), wire2_r_4_im, wire2_r_5_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_11 = mux(UInt<1>("h1"), wire2_r_4_re, wire2_r_5_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_6_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_6_re) @[Reg.scala 27:20]
    reg wire2_r_6_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_6_im) @[Reg.scala 27:20]
    node _GEN_12 = mux(UInt<1>("h1"), wire2_r_5_im, wire2_r_6_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_13 = mux(UInt<1>("h1"), wire2_r_5_re, wire2_r_6_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_7_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_re) @[Reg.scala 27:20]
    reg wire2_r_7_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_im) @[Reg.scala 27:20]
    node _GEN_14 = mux(UInt<1>("h1"), wire2_r_6_im, wire2_r_7_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_15 = mux(UInt<1>("h1"), wire2_r_6_re, wire2_r_7_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_8_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_8_re) @[Reg.scala 27:20]
    reg wire2_r_8_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_8_im) @[Reg.scala 27:20]
    node _GEN_16 = mux(UInt<1>("h1"), wire2_r_7_im, wire2_r_8_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_17 = mux(UInt<1>("h1"), wire2_r_7_re, wire2_r_8_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_9_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_9_re) @[Reg.scala 27:20]
    reg wire2_r_9_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_9_im) @[Reg.scala 27:20]
    node _GEN_18 = mux(UInt<1>("h1"), wire2_r_8_im, wire2_r_9_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_19 = mux(UInt<1>("h1"), wire2_r_8_re, wire2_r_9_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_10_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_10_re) @[Reg.scala 27:20]
    reg wire2_r_10_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_10_im) @[Reg.scala 27:20]
    node _GEN_20 = mux(UInt<1>("h1"), wire2_r_9_im, wire2_r_10_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_21 = mux(UInt<1>("h1"), wire2_r_9_re, wire2_r_10_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_11_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_11_re) @[Reg.scala 27:20]
    reg wire2_r_11_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_11_im) @[Reg.scala 27:20]
    node _GEN_22 = mux(UInt<1>("h1"), wire2_r_10_im, wire2_r_11_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_23 = mux(UInt<1>("h1"), wire2_r_10_re, wire2_r_11_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_12_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_12_re) @[Reg.scala 27:20]
    reg wire2_r_12_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_12_im) @[Reg.scala 27:20]
    node _GEN_24 = mux(UInt<1>("h1"), wire2_r_11_im, wire2_r_12_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_25 = mux(UInt<1>("h1"), wire2_r_11_re, wire2_r_12_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_13_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_13_re) @[Reg.scala 27:20]
    reg wire2_r_13_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_13_im) @[Reg.scala 27:20]
    node _GEN_26 = mux(UInt<1>("h1"), wire2_r_12_im, wire2_r_13_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_27 = mux(UInt<1>("h1"), wire2_r_12_re, wire2_r_13_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    reg wire2_r_14_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_14_re) @[Reg.scala 27:20]
    reg wire2_r_14_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_14_im) @[Reg.scala 27:20]
    node _GEN_28 = mux(UInt<1>("h1"), wire2_r_13_im, wire2_r_14_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_29 = mux(UInt<1>("h1"), wire2_r_13_re, wire2_r_14_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_30 = mux(UInt<1>("h1"), wire2_r_14_im, wire2_r_15_im) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _GEN_31 = mux(UInt<1>("h1"), wire2_r_14_re, wire2_r_15_re) @[Reg.scala 28:19 Reg.scala 28:23 Reg.scala 27:20]
    node _wire2_WIRE_re = _wire2_T_1 @[SDC_SDF_combFFT.scala 113:79 SDC_SDF_combFFT.scala 113:79]
    node _wire2_WIRE_im = _wire2_T @[SDC_SDF_combFFT.scala 113:79 SDC_SDF_combFFT.scala 113:79]
    io_out_re <= _io_out_T_re @[SDC_SDF_combFFT.scala 112:12]
    io_out_im <= _io_out_T_im @[SDC_SDF_combFFT.scala 112:12]
    wire1_inst.clock <= clock
    wire1_inst.reset <= io_altRst
    wire1_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 41:17]
    wire1_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 41:17]
    wire1_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 42:17]
    wire1_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 42:17]
    io_out_inst.clock <= clock
    io_out_inst.reset <= io_altRst
    io_out_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 58:17]
    io_out_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 58:17]
    io_out_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 59:17]
    io_out_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 59:17]
    wire2_r_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_1) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_0) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_1_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_3) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_1_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_2) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_2_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_5) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_2_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_4) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_3_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_7) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_3_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_6) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_4_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_9) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_4_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_8) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_5_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_11) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_5_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_10) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_6_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_13) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_6_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_12) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_7_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_15) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_7_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_14) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_8_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_17) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_8_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_16) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_9_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_19) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_9_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_18) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_10_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_21) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_10_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_20) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_11_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_23) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_11_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_22) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_12_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_25) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_12_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_24) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_13_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_27) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_13_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_26) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_14_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_29) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_14_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_28) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_15_re <= mux(io_altRst, _wire2_WIRE_re, _GEN_31) @[Reg.scala 27:20 Reg.scala 27:20]
    wire2_r_15_im <= mux(io_altRst, _wire2_WIRE_im, _GEN_30) @[Reg.scala 27:20 Reg.scala 27:20]

  module Reorder :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>
    input io_inValid : UInt<1>
    output io_outValid : UInt<1>

    mem ram_re : @[SDC_SDF_combFFT.scala 246:16]
      data-type => SInt<32>
      depth => 32
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      writer => MPORT_1
      read-under-write => undefined
    mem ram_im : @[SDC_SDF_combFFT.scala 246:16]
      data-type => SInt<32>
      depth => 32
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      writer => MPORT_1
      read-under-write => undefined
    reg inCounter : UInt<5>, clock with :
      reset => (UInt<1>("h0"), inCounter) @[SDC_SDF_combFFT.scala 248:26]
    reg inCounterCut : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inCounterCut) @[SDC_SDF_combFFT.scala 249:29]
    reg outCounter : UInt<5>, clock with :
      reset => (UInt<1>("h0"), outCounter) @[SDC_SDF_combFFT.scala 250:27]
    node _index0_T = bits(inCounterCut, 1, 0) @[Bitwise.scala 109:18]
    node index0_hi = bits(_index0_T, 0, 0) @[Bitwise.scala 109:18]
    node index0_lo = bits(_index0_T, 1, 1) @[Bitwise.scala 109:44]
    node index0_hi_1 = cat(index0_hi, index0_lo) @[Cat.scala 30:58]
    node _index0_T_1 = bits(inCounterCut, 3, 2) @[Bitwise.scala 109:44]
    node index0_hi_2 = bits(_index0_T_1, 0, 0) @[Bitwise.scala 109:18]
    node index0_lo_1 = bits(_index0_T_1, 1, 1) @[Bitwise.scala 109:44]
    node index0_lo_2 = cat(index0_hi_2, index0_lo_1) @[Cat.scala 30:58]
    node index0 = cat(index0_hi_1, index0_lo_2) @[Cat.scala 30:58]
    node _index1_T = bits(inCounterCut, 1, 0) @[Bitwise.scala 109:18]
    node index1_hi = bits(_index1_T, 0, 0) @[Bitwise.scala 109:18]
    node index1_lo = bits(_index1_T, 1, 1) @[Bitwise.scala 109:44]
    node index1_hi_1 = cat(index1_hi, index1_lo) @[Cat.scala 30:58]
    node _index1_T_1 = bits(inCounterCut, 3, 2) @[Bitwise.scala 109:44]
    node index1_hi_2 = bits(_index1_T_1, 0, 0) @[Bitwise.scala 109:18]
    node index1_lo_1 = bits(_index1_T_1, 1, 1) @[Bitwise.scala 109:44]
    node index1_lo_2 = cat(index1_hi_2, index1_lo_1) @[Cat.scala 30:58]
    node _index1_T_2 = cat(index1_hi_1, index1_lo_2) @[Cat.scala 30:58]
    node _index1_T_3 = add(_index1_T_2, UInt<5>("h10")) @[SDC_SDF_combFFT.scala 252:38]
    node index1 = tail(_index1_T_3, 1) @[SDC_SDF_combFFT.scala 252:38]
    node _T = neq(inCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:33]
    node _T_1 = or(io_inValid, _T) @[SDC_SDF_combFFT.scala 253:20]
    node _T_2 = lt(inCounter, UInt<5>("h10")) @[SDC_SDF_combFFT.scala 254:21]
    node _GEN_0 = validif(_T_2, index1) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_1 = validif(_T_2, clock) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_2 = mux(_T_2, UInt<1>("h1"), UInt<1>("h0")) @[SDC_SDF_combFFT.scala 254:42 SDC_SDF_combFFT.scala 246:16]
    node _GEN_3 = validif(_T_2, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_4 = validif(_T_2, io_in_im) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_5 = validif(_T_2, io_in_re) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_6 = validif(eq(_T_2, UInt<1>("h0")), index0) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_7 = validif(eq(_T_2, UInt<1>("h0")), clock) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_8 = mux(_T_2, UInt<1>("h0"), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 254:42 SDC_SDF_combFFT.scala 246:16]
    node _GEN_9 = validif(eq(_T_2, UInt<1>("h0")), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_10 = validif(eq(_T_2, UInt<1>("h0")), io_in_im) @[SDC_SDF_combFFT.scala 254:42]
    node _GEN_11 = validif(eq(_T_2, UInt<1>("h0")), io_in_re) @[SDC_SDF_combFFT.scala 254:42]
    node _inCounter_T = add(inCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 259:28]
    node _inCounter_T_1 = tail(_inCounter_T, 1) @[SDC_SDF_combFFT.scala 259:28]
    node _inCounterCut_T = add(inCounterCut, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 260:34]
    node _inCounterCut_T_1 = tail(_inCounterCut_T, 1) @[SDC_SDF_combFFT.scala 260:34]
    node _GEN_12 = validif(_T_1, _GEN_0) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_13 = validif(_T_1, _GEN_1) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_14 = mux(_T_1, _GEN_2, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:42 SDC_SDF_combFFT.scala 246:16]
    node _GEN_15 = validif(_T_1, _GEN_3) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_16 = validif(_T_1, _GEN_4) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_17 = validif(_T_1, _GEN_5) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_18 = validif(_T_1, _GEN_6) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_19 = validif(_T_1, _GEN_7) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_20 = mux(_T_1, _GEN_8, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:42 SDC_SDF_combFFT.scala 246:16]
    node _GEN_21 = validif(_T_1, _GEN_9) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_22 = validif(_T_1, _GEN_10) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_23 = validif(_T_1, _GEN_11) @[SDC_SDF_combFFT.scala 253:42]
    node _GEN_24 = mux(_T_1, _inCounter_T_1, inCounter) @[SDC_SDF_combFFT.scala 253:42 SDC_SDF_combFFT.scala 259:15 SDC_SDF_combFFT.scala 248:26]
    node _GEN_25 = mux(_T_1, _inCounterCut_T_1, inCounterCut) @[SDC_SDF_combFFT.scala 253:42 SDC_SDF_combFFT.scala 260:18 SDC_SDF_combFFT.scala 249:29]
    reg io_outValid_REG : UInt<5>, clock with :
      reset => (UInt<1>("h0"), io_outValid_REG) @[SDC_SDF_combFFT.scala 262:26]
    node _io_outValid_T = eq(io_outValid_REG, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 262:38]
    node _io_outValid_T_1 = neq(outCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 262:76]
    node _io_outValid_T_2 = or(_io_outValid_T, _io_outValid_T_1) @[SDC_SDF_combFFT.scala 262:61]
    node _outCounter_T = add(outCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 264:30]
    node _outCounter_T_1 = tail(_outCounter_T, 1) @[SDC_SDF_combFFT.scala 264:30]
    node _GEN_26 = mux(io_outValid, _outCounter_T_1, outCounter) @[SDC_SDF_combFFT.scala 263:22 SDC_SDF_combFFT.scala 264:16 SDC_SDF_combFFT.scala 250:27]
    io_out_re <= ram_re.io_out_MPORT.data @[SDC_SDF_combFFT.scala 266:10]
    io_out_im <= ram_im.io_out_MPORT.data @[SDC_SDF_combFFT.scala 266:10]
    io_outValid <= _io_outValid_T_2 @[SDC_SDF_combFFT.scala 262:15]
    ram_re.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 266:21]
    ram_im.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 266:21]
    ram_re.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 266:21]
    ram_im.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 266:21]
    ram_re.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 266:21]
    ram_im.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 266:21]
    ram_re.MPORT.addr <= _GEN_12
    ram_im.MPORT.addr <= _GEN_12
    ram_re.MPORT.en <= _GEN_14
    ram_im.MPORT.en <= _GEN_14
    ram_re.MPORT.clk <= _GEN_13
    ram_im.MPORT.clk <= _GEN_13
    ram_re.MPORT.data <= _GEN_17
    ram_im.MPORT.data <= _GEN_16
    ram_re.MPORT.mask <= _GEN_15
    ram_im.MPORT.mask <= _GEN_15
    ram_re.MPORT_1.addr <= _GEN_18
    ram_im.MPORT_1.addr <= _GEN_18
    ram_re.MPORT_1.en <= _GEN_20
    ram_im.MPORT_1.en <= _GEN_20
    ram_re.MPORT_1.clk <= _GEN_19
    ram_im.MPORT_1.clk <= _GEN_19
    ram_re.MPORT_1.data <= _GEN_23
    ram_im.MPORT_1.data <= _GEN_22
    ram_re.MPORT_1.mask <= _GEN_21
    ram_im.MPORT_1.mask <= _GEN_21
    inCounter <= mux(reset, UInt<5>("h0"), _GEN_24) @[SDC_SDF_combFFT.scala 248:26 SDC_SDF_combFFT.scala 248:26]
    inCounterCut <= mux(reset, UInt<4>("h0"), _GEN_25) @[SDC_SDF_combFFT.scala 249:29 SDC_SDF_combFFT.scala 249:29]
    outCounter <= mux(reset, UInt<5>("h0"), _GEN_26) @[SDC_SDF_combFFT.scala 250:27 SDC_SDF_combFFT.scala 250:27]
    io_outValid_REG <= inCounter @[SDC_SDF_combFFT.scala 262:26]

  module CombFFT :
    input clock : Clock
    input reset : UInt<1>
    input io_dataIn_re : SInt<32>
    input io_dataIn_im : SInt<32>
    input io_inValid : UInt<1>
    output io_dataOut_re : SInt<32>
    output io_dataOut_im : SInt<32>
    output io_outValid : UInt<1>
    output io_busy : UInt<1>

    inst inst of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst inst_1 of Commutator_1 @[SDC_SDF_combFFT.scala 86:22]
    inst inst_2 of RealAddSub @[SDC_SDF_combFFT.scala 144:22]
    inst inst_3 of RealBranchMul @[SDC_SDF_combFFT.scala 198:22]
    inst inst_4 of Switch @[SDC_SDF_combFFT.scala 174:22]
    inst inst_5 of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst stageIntf2_1_inst of RealAdder @[SDC_SDF_combFFT.scala 226:22]
    inst inst_6 of Commutator_3 @[SDC_SDF_combFFT.scala 86:22]
    inst inst_7 of RealAddSub @[SDC_SDF_combFFT.scala 144:22]
    inst inst_8 of RealBranchMul @[SDC_SDF_combFFT.scala 198:22]
    inst inst_9 of Switch @[SDC_SDF_combFFT.scala 174:22]
    inst inst_10 of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst stageIntf2_2_inst of RealAdder @[SDC_SDF_combFFT.scala 226:22]
    inst inst_11 of Commutator_5 @[SDC_SDF_combFFT.scala 86:22]
    inst inst_12 of RealAddSub @[SDC_SDF_combFFT.scala 144:22]
    inst inst_13 of RealBranchMul @[SDC_SDF_combFFT.scala 198:22]
    inst inst_14 of Switch @[SDC_SDF_combFFT.scala 174:22]
    inst inst_15 of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst stageIntf2_3_inst of RealAdder @[SDC_SDF_combFFT.scala 226:22]
    inst inst_16 of Commutator_7 @[SDC_SDF_combFFT.scala 86:22]
    inst inst_17 of RealAddSub @[SDC_SDF_combFFT.scala 144:22]
    inst inst_18 of RealBranchMul @[SDC_SDF_combFFT.scala 198:22]
    inst inst_19 of Switch @[SDC_SDF_combFFT.scala 174:22]
    inst inst_20 of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst stageIntf2_4_inst of RealAdder @[SDC_SDF_combFFT.scala 226:22]
    inst inst_21 of Commutator @[SDC_SDF_combFFT.scala 86:22]
    inst sdfReturn_inst of SDFUnit @[SDC_SDF_combFFT.scala 120:22]
    inst reorderModule of Reorder @[SDC_SDF_combFFT.scala 360:29]
    reg dCount : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dCount) @[SDC_SDF_combFFT.scala 304:23]
    node busy = neq(dCount, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 305:21]
    node _T = or(io_inValid, busy) @[SDC_SDF_combFFT.scala 306:19]
    node _dCount_T = eq(dCount, UInt<7>("h73")) @[SDC_SDF_combFFT.scala 307:26]
    node _dCount_T_1 = add(dCount, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 307:77]
    node _dCount_T_2 = tail(_dCount_T_1, 1) @[SDC_SDF_combFFT.scala 307:77]
    node _dCount_T_3 = mux(_dCount_T, UInt<1>("h0"), _dCount_T_2) @[SDC_SDF_combFFT.scala 307:18]
    node _GEN_0 = mux(_T, _dCount_T_3, dCount) @[SDC_SDF_combFFT.scala 306:27 SDC_SDF_combFFT.scala 307:12 SDC_SDF_combFFT.scala 304:23]
    node _T_1 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 313:71]
    node _T_2 = bits(_T_1, 0, 0) @[SDC_SDF_combFFT.scala 313:81]
    node _T_3 = sub(dCount, UInt<5>("h11")) @[SDC_SDF_combFFT.scala 321:42]
    node _T_4 = tail(_T_3, 1) @[SDC_SDF_combFFT.scala 321:42]
    node _T_5 = bits(_T_4, 4, 0)
    node _wireWRe_T = geq(dCount, UInt<5>("h11")) @[SDC_SDF_combFFT.scala 323:27]
    node _wireWRe_T_1 = lt(dCount, UInt<6>("h31")) @[SDC_SDF_combFFT.scala 323:44]
    node _wireWRe_T_2 = and(_wireWRe_T, _wireWRe_T_1) @[SDC_SDF_combFFT.scala 323:34]
    node _WIRE__0 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_1 = validif(eq(UInt<1>("h0"), _T_5), _WIRE__0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__1 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_2 = mux(eq(UInt<1>("h1"), _T_5), _WIRE__1, _GEN_1) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__2 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_3 = mux(eq(UInt<2>("h2"), _T_5), _WIRE__2, _GEN_2) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__3 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_4 = mux(eq(UInt<2>("h3"), _T_5), _WIRE__3, _GEN_3) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__4 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_5 = mux(eq(UInt<3>("h4"), _T_5), _WIRE__4, _GEN_4) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__5 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_6 = mux(eq(UInt<3>("h5"), _T_5), _WIRE__5, _GEN_5) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__6 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_7 = mux(eq(UInt<3>("h6"), _T_5), _WIRE__6, _GEN_6) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__7 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_8 = mux(eq(UInt<3>("h7"), _T_5), _WIRE__7, _GEN_7) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__8 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_9 = mux(eq(UInt<4>("h8"), _T_5), _WIRE__8, _GEN_8) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__9 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_10 = mux(eq(UInt<4>("h9"), _T_5), _WIRE__9, _GEN_9) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__10 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_11 = mux(eq(UInt<4>("ha"), _T_5), _WIRE__10, _GEN_10) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__11 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_12 = mux(eq(UInt<4>("hb"), _T_5), _WIRE__11, _GEN_11) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__12 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_13 = mux(eq(UInt<4>("hc"), _T_5), _WIRE__12, _GEN_12) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__13 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_14 = mux(eq(UInt<4>("hd"), _T_5), _WIRE__13, _GEN_13) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__14 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_15 = mux(eq(UInt<4>("he"), _T_5), _WIRE__14, _GEN_14) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__15 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_16 = mux(eq(UInt<4>("hf"), _T_5), _WIRE__15, _GEN_15) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__16 = pad(UInt<1>("h1"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_17 = mux(eq(UInt<5>("h10"), _T_5), _WIRE__16, _GEN_16) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__17 = pad(UInt<1>("h1"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_18 = mux(eq(UInt<5>("h11"), _T_5), _WIRE__17, _GEN_17) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__18 = pad(UInt<2>("h3"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_19 = mux(eq(UInt<5>("h12"), _T_5), _WIRE__18, _GEN_18) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__19 = pad(UInt<2>("h3"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_20 = mux(eq(UInt<5>("h13"), _T_5), _WIRE__19, _GEN_19) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__20 = pad(UInt<3>("h5"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_21 = mux(eq(UInt<5>("h14"), _T_5), _WIRE__20, _GEN_20) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__21 = pad(UInt<3>("h5"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_22 = mux(eq(UInt<5>("h15"), _T_5), _WIRE__21, _GEN_21) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__22 = pad(UInt<3>("h7"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_23 = mux(eq(UInt<5>("h16"), _T_5), _WIRE__22, _GEN_22) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__23 = pad(UInt<3>("h7"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_24 = mux(eq(UInt<5>("h17"), _T_5), _WIRE__23, _GEN_23) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__24 = UInt<4>("h9") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_25 = mux(eq(UInt<5>("h18"), _T_5), _WIRE__24, _GEN_24) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__25 = UInt<4>("h9") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_26 = mux(eq(UInt<5>("h19"), _T_5), _WIRE__25, _GEN_25) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__26 = UInt<4>("hb") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_27 = mux(eq(UInt<5>("h1a"), _T_5), _WIRE__26, _GEN_26) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__27 = UInt<4>("hb") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_28 = mux(eq(UInt<5>("h1b"), _T_5), _WIRE__27, _GEN_27) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__28 = UInt<4>("hd") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_29 = mux(eq(UInt<5>("h1c"), _T_5), _WIRE__28, _GEN_28) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__29 = UInt<4>("hd") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_30 = mux(eq(UInt<5>("h1d"), _T_5), _WIRE__29, _GEN_29) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__30 = UInt<4>("hf") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_31 = mux(eq(UInt<5>("h1e"), _T_5), _WIRE__30, _GEN_30) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE__31 = UInt<4>("hf") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_32 = mux(eq(UInt<5>("h1f"), _T_5), _WIRE__31, _GEN_31) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_T_5 = _GEN_32 @[SDC_SDF_combFFT.scala 323:19]
    node re_s__0 = asSInt(UInt<32>("h10000")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_33 = validif(eq(UInt<1>("h0"), _WIRE_T_5), re_s__0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__1 = asSInt(UInt<32>("hfb15")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_34 = mux(eq(UInt<1>("h1"), _WIRE_T_5), re_s__1, _GEN_33) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__2 = asSInt(UInt<32>("hec83")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_35 = mux(eq(UInt<2>("h2"), _WIRE_T_5), re_s__2, _GEN_34) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__3 = asSInt(UInt<32>("hd4db")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_36 = mux(eq(UInt<2>("h3"), _WIRE_T_5), re_s__3, _GEN_35) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__4 = asSInt(UInt<32>("hb505")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_37 = mux(eq(UInt<3>("h4"), _WIRE_T_5), re_s__4, _GEN_36) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__5 = asSInt(UInt<32>("h8e3a")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_38 = mux(eq(UInt<3>("h5"), _WIRE_T_5), re_s__5, _GEN_37) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__6 = asSInt(UInt<32>("h61f8")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_39 = mux(eq(UInt<3>("h6"), _WIRE_T_5), re_s__6, _GEN_38) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__7 = asSInt(UInt<32>("h31f1")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_40 = mux(eq(UInt<3>("h7"), _WIRE_T_5), re_s__7, _GEN_39) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__8 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_41 = mux(eq(UInt<4>("h8"), _WIRE_T_5), re_s__8, _GEN_40) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__9 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_42 = mux(eq(UInt<4>("h9"), _WIRE_T_5), re_s__9, _GEN_41) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__10 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_43 = mux(eq(UInt<4>("ha"), _WIRE_T_5), re_s__10, _GEN_42) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__11 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_44 = mux(eq(UInt<4>("hb"), _WIRE_T_5), re_s__11, _GEN_43) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_45 = mux(eq(UInt<4>("hc"), _WIRE_T_5), re_s__12, _GEN_44) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__13 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_46 = mux(eq(UInt<4>("hd"), _WIRE_T_5), re_s__13, _GEN_45) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__14 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_47 = mux(eq(UInt<4>("he"), _WIRE_T_5), re_s__14, _GEN_46) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s__15 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_48 = mux(eq(UInt<4>("hf"), _WIRE_T_5), re_s__15, _GEN_47) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _re_s_WIRE_T_5 = _GEN_48 @[SDC_SDF_combFFT.scala 323:19]
    node _wireWRe_T_3 = mux(_wireWRe_T_2, _re_s_WIRE_T_5, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 323:19]
    node _wireWIm_T = geq(dCount, UInt<5>("h11")) @[SDC_SDF_combFFT.scala 324:27]
    node _wireWIm_T_1 = lt(dCount, UInt<6>("h31")) @[SDC_SDF_combFFT.scala 324:44]
    node _wireWIm_T_2 = and(_wireWIm_T, _wireWIm_T_1) @[SDC_SDF_combFFT.scala 324:34]
    node _WIRE_T_5_0 = _GEN_32 @[SDC_SDF_combFFT.scala 324:19]
    node im_s__0 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_49 = validif(eq(UInt<1>("h0"), _WIRE_T_5_0), im_s__0) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__1 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_50 = mux(eq(UInt<1>("h1"), _WIRE_T_5_0), im_s__1, _GEN_49) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__2 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_51 = mux(eq(UInt<2>("h2"), _WIRE_T_5_0), im_s__2, _GEN_50) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__3 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_52 = mux(eq(UInt<2>("h3"), _WIRE_T_5_0), im_s__3, _GEN_51) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__4 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_53 = mux(eq(UInt<3>("h4"), _WIRE_T_5_0), im_s__4, _GEN_52) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__5 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_54 = mux(eq(UInt<3>("h5"), _WIRE_T_5_0), im_s__5, _GEN_53) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__6 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_55 = mux(eq(UInt<3>("h6"), _WIRE_T_5_0), im_s__6, _GEN_54) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__7 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_56 = mux(eq(UInt<3>("h7"), _WIRE_T_5_0), im_s__7, _GEN_55) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__8 = asSInt(UInt<32>("hffff0000")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_57 = mux(eq(UInt<4>("h8"), _WIRE_T_5_0), im_s__8, _GEN_56) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__9 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_58 = mux(eq(UInt<4>("h9"), _WIRE_T_5_0), im_s__9, _GEN_57) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__10 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_59 = mux(eq(UInt<4>("ha"), _WIRE_T_5_0), im_s__10, _GEN_58) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__11 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_60 = mux(eq(UInt<4>("hb"), _WIRE_T_5_0), im_s__11, _GEN_59) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_61 = mux(eq(UInt<4>("hc"), _WIRE_T_5_0), im_s__12, _GEN_60) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__13 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_62 = mux(eq(UInt<4>("hd"), _WIRE_T_5_0), im_s__13, _GEN_61) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__14 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_63 = mux(eq(UInt<4>("he"), _WIRE_T_5_0), im_s__14, _GEN_62) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s__15 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_64 = mux(eq(UInt<4>("hf"), _WIRE_T_5_0), im_s__15, _GEN_63) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node _im_s_WIRE_T_5_0 = _GEN_64 @[SDC_SDF_combFFT.scala 324:19]
    node _wireWIm_T_3 = mux(_wireWIm_T_2, _im_s_WIRE_T_5_0, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 324:19]
    node _wireSelectDC_T = geq(dCount, UInt<5>("h11")) @[SDC_SDF_combFFT.scala 329:32]
    node _wireSelectDC_T_1 = sub(dCount, UInt<5>("h11")) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_2 = tail(_wireSelectDC_T_1, 1) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_3 = bits(_wireSelectDC_T_2, 4, 4) @[SDC_SDF_combFFT.scala 329:53]
    node _wireSelectDC_T_4 = bits(_wireSelectDC_T_3, 0, 0) @[SDC_SDF_combFFT.scala 329:77]
    node _wireSelectDC_T_5 = eq(_wireSelectDC_T_4, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:40]
    node _wireSelectDC_T_6 = mux(_wireSelectDC_T, _wireSelectDC_T_5, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:24]
    node _commonSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:51]
    node _commonSelt_T_1 = bits(_commonSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 337:61]
    node _commonSelt_T_2 = eq(_commonSelt_T_1, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 337:44]
    node _commonSelt_T_3 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:71]
    node _commonSelt_T_4 = bits(_commonSelt_T_3, 0, 0) @[SDC_SDF_combFFT.scala 337:81]
    node _commonSelt_T_5 = mux(UInt<1>("h1"), _commonSelt_T_2, _commonSelt_T_4) @[SDC_SDF_combFFT.scala 337:22]
    reg stageIntf1_1_REG : SInt<32>, clock with :
      reset => (UInt<1>("h0"), stageIntf1_1_REG) @[SDC_SDF_combFFT.scala 344:33]
    node _T_6 = sub(dCount, UInt<5>("h1a")) @[SDC_SDF_combFFT.scala 321:42]
    node _T_7 = tail(_T_6, 1) @[SDC_SDF_combFFT.scala 321:42]
    node _T_8 = bits(_T_7, 4, 0)
    node _wireWRe_T_4 = geq(dCount, UInt<5>("h1a")) @[SDC_SDF_combFFT.scala 323:27]
    node _wireWRe_T_5 = lt(dCount, UInt<6>("h3a")) @[SDC_SDF_combFFT.scala 323:44]
    node _wireWRe_T_6 = and(_wireWRe_T_4, _wireWRe_T_5) @[SDC_SDF_combFFT.scala 323:34]
    node _WIRE_1_0 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_65 = validif(eq(UInt<1>("h0"), _T_8), _WIRE_1_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_1 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_66 = mux(eq(UInt<1>("h1"), _T_8), _WIRE_1_1, _GEN_65) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_2 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_67 = mux(eq(UInt<2>("h2"), _T_8), _WIRE_1_2, _GEN_66) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_3 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_68 = mux(eq(UInt<2>("h3"), _T_8), _WIRE_1_3, _GEN_67) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_4 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_69 = mux(eq(UInt<3>("h4"), _T_8), _WIRE_1_4, _GEN_68) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_5 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_70 = mux(eq(UInt<3>("h5"), _T_8), _WIRE_1_5, _GEN_69) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_6 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_71 = mux(eq(UInt<3>("h6"), _T_8), _WIRE_1_6, _GEN_70) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_7 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_72 = mux(eq(UInt<3>("h7"), _T_8), _WIRE_1_7, _GEN_71) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_8 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_73 = mux(eq(UInt<4>("h8"), _T_8), _WIRE_1_8, _GEN_72) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_9 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_74 = mux(eq(UInt<4>("h9"), _T_8), _WIRE_1_9, _GEN_73) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_10 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_75 = mux(eq(UInt<4>("ha"), _T_8), _WIRE_1_10, _GEN_74) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_11 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_76 = mux(eq(UInt<4>("hb"), _T_8), _WIRE_1_11, _GEN_75) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_12 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_77 = mux(eq(UInt<4>("hc"), _T_8), _WIRE_1_12, _GEN_76) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_13 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_78 = mux(eq(UInt<4>("hd"), _T_8), _WIRE_1_13, _GEN_77) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_14 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_79 = mux(eq(UInt<4>("he"), _T_8), _WIRE_1_14, _GEN_78) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_15 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_80 = mux(eq(UInt<4>("hf"), _T_8), _WIRE_1_15, _GEN_79) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_16 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_81 = mux(eq(UInt<5>("h10"), _T_8), _WIRE_1_16, _GEN_80) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_17 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_82 = mux(eq(UInt<5>("h11"), _T_8), _WIRE_1_17, _GEN_81) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_18 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_83 = mux(eq(UInt<5>("h12"), _T_8), _WIRE_1_18, _GEN_82) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_19 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_84 = mux(eq(UInt<5>("h13"), _T_8), _WIRE_1_19, _GEN_83) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_20 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_85 = mux(eq(UInt<5>("h14"), _T_8), _WIRE_1_20, _GEN_84) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_21 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_86 = mux(eq(UInt<5>("h15"), _T_8), _WIRE_1_21, _GEN_85) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_22 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_87 = mux(eq(UInt<5>("h16"), _T_8), _WIRE_1_22, _GEN_86) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_23 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_88 = mux(eq(UInt<5>("h17"), _T_8), _WIRE_1_23, _GEN_87) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_24 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_89 = mux(eq(UInt<5>("h18"), _T_8), _WIRE_1_24, _GEN_88) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_25 = pad(UInt<2>("h2"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_90 = mux(eq(UInt<5>("h19"), _T_8), _WIRE_1_25, _GEN_89) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_26 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_91 = mux(eq(UInt<5>("h1a"), _T_8), _WIRE_1_26, _GEN_90) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_27 = pad(UInt<3>("h6"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_92 = mux(eq(UInt<5>("h1b"), _T_8), _WIRE_1_27, _GEN_91) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_28 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_93 = mux(eq(UInt<5>("h1c"), _T_8), _WIRE_1_28, _GEN_92) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_29 = UInt<4>("ha") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_94 = mux(eq(UInt<5>("h1d"), _T_8), _WIRE_1_29, _GEN_93) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_30 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_95 = mux(eq(UInt<5>("h1e"), _T_8), _WIRE_1_30, _GEN_94) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_31 = UInt<4>("he") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_96 = mux(eq(UInt<5>("h1f"), _T_8), _WIRE_1_31, _GEN_95) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_1_T_8 = _GEN_96 @[SDC_SDF_combFFT.scala 323:19]
    node re_s_1_0 = asSInt(UInt<32>("h10000")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_97 = validif(eq(UInt<1>("h0"), _WIRE_1_T_8), re_s_1_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_1 = asSInt(UInt<32>("hfb15")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_98 = mux(eq(UInt<1>("h1"), _WIRE_1_T_8), re_s_1_1, _GEN_97) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_2 = asSInt(UInt<32>("hec83")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_99 = mux(eq(UInt<2>("h2"), _WIRE_1_T_8), re_s_1_2, _GEN_98) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_3 = asSInt(UInt<32>("hd4db")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_100 = mux(eq(UInt<2>("h3"), _WIRE_1_T_8), re_s_1_3, _GEN_99) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_4 = asSInt(UInt<32>("hb505")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_101 = mux(eq(UInt<3>("h4"), _WIRE_1_T_8), re_s_1_4, _GEN_100) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_5 = asSInt(UInt<32>("h8e3a")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_102 = mux(eq(UInt<3>("h5"), _WIRE_1_T_8), re_s_1_5, _GEN_101) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_6 = asSInt(UInt<32>("h61f8")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_103 = mux(eq(UInt<3>("h6"), _WIRE_1_T_8), re_s_1_6, _GEN_102) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_7 = asSInt(UInt<32>("h31f1")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_104 = mux(eq(UInt<3>("h7"), _WIRE_1_T_8), re_s_1_7, _GEN_103) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_8 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_105 = mux(eq(UInt<4>("h8"), _WIRE_1_T_8), re_s_1_8, _GEN_104) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_9 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_106 = mux(eq(UInt<4>("h9"), _WIRE_1_T_8), re_s_1_9, _GEN_105) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_10 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_107 = mux(eq(UInt<4>("ha"), _WIRE_1_T_8), re_s_1_10, _GEN_106) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_11 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_108 = mux(eq(UInt<4>("hb"), _WIRE_1_T_8), re_s_1_11, _GEN_107) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_109 = mux(eq(UInt<4>("hc"), _WIRE_1_T_8), re_s_1_12, _GEN_108) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_13 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_110 = mux(eq(UInt<4>("hd"), _WIRE_1_T_8), re_s_1_13, _GEN_109) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_14 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_111 = mux(eq(UInt<4>("he"), _WIRE_1_T_8), re_s_1_14, _GEN_110) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_1_15 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_112 = mux(eq(UInt<4>("hf"), _WIRE_1_T_8), re_s_1_15, _GEN_111) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _re_s_1_WIRE_1_T_8 = _GEN_112 @[SDC_SDF_combFFT.scala 323:19]
    node _wireWRe_T_7 = mux(_wireWRe_T_6, _re_s_1_WIRE_1_T_8, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 323:19]
    node _wireWIm_T_4 = geq(dCount, UInt<5>("h1a")) @[SDC_SDF_combFFT.scala 324:27]
    node _wireWIm_T_5 = lt(dCount, UInt<6>("h3a")) @[SDC_SDF_combFFT.scala 324:44]
    node _wireWIm_T_6 = and(_wireWIm_T_4, _wireWIm_T_5) @[SDC_SDF_combFFT.scala 324:34]
    node _WIRE_1_T_8_0 = _GEN_96 @[SDC_SDF_combFFT.scala 324:19]
    node im_s_1_0 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_113 = validif(eq(UInt<1>("h0"), _WIRE_1_T_8_0), im_s_1_0) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_1 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_114 = mux(eq(UInt<1>("h1"), _WIRE_1_T_8_0), im_s_1_1, _GEN_113) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_2 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_115 = mux(eq(UInt<2>("h2"), _WIRE_1_T_8_0), im_s_1_2, _GEN_114) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_3 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_116 = mux(eq(UInt<2>("h3"), _WIRE_1_T_8_0), im_s_1_3, _GEN_115) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_4 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_117 = mux(eq(UInt<3>("h4"), _WIRE_1_T_8_0), im_s_1_4, _GEN_116) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_5 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_118 = mux(eq(UInt<3>("h5"), _WIRE_1_T_8_0), im_s_1_5, _GEN_117) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_6 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_119 = mux(eq(UInt<3>("h6"), _WIRE_1_T_8_0), im_s_1_6, _GEN_118) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_7 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_120 = mux(eq(UInt<3>("h7"), _WIRE_1_T_8_0), im_s_1_7, _GEN_119) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_8 = asSInt(UInt<32>("hffff0000")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_121 = mux(eq(UInt<4>("h8"), _WIRE_1_T_8_0), im_s_1_8, _GEN_120) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_9 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_122 = mux(eq(UInt<4>("h9"), _WIRE_1_T_8_0), im_s_1_9, _GEN_121) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_10 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_123 = mux(eq(UInt<4>("ha"), _WIRE_1_T_8_0), im_s_1_10, _GEN_122) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_11 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_124 = mux(eq(UInt<4>("hb"), _WIRE_1_T_8_0), im_s_1_11, _GEN_123) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_125 = mux(eq(UInt<4>("hc"), _WIRE_1_T_8_0), im_s_1_12, _GEN_124) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_13 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_126 = mux(eq(UInt<4>("hd"), _WIRE_1_T_8_0), im_s_1_13, _GEN_125) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_14 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_127 = mux(eq(UInt<4>("he"), _WIRE_1_T_8_0), im_s_1_14, _GEN_126) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_1_15 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_128 = mux(eq(UInt<4>("hf"), _WIRE_1_T_8_0), im_s_1_15, _GEN_127) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node _im_s_1_WIRE_1_T_8_0 = _GEN_128 @[SDC_SDF_combFFT.scala 324:19]
    node _wireWIm_T_7 = mux(_wireWIm_T_6, _im_s_1_WIRE_1_T_8_0, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 324:19]
    node _wireSelectDC_T_7 = geq(dCount, UInt<5>("h1a")) @[SDC_SDF_combFFT.scala 329:32]
    node _wireSelectDC_T_8 = sub(dCount, UInt<5>("h1a")) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_9 = tail(_wireSelectDC_T_8, 1) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_10 = bits(_wireSelectDC_T_9, 3, 3) @[SDC_SDF_combFFT.scala 329:53]
    node _wireSelectDC_T_11 = bits(_wireSelectDC_T_10, 0, 0) @[SDC_SDF_combFFT.scala 329:77]
    node _wireSelectDC_T_12 = eq(_wireSelectDC_T_11, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:40]
    node _wireSelectDC_T_13 = mux(_wireSelectDC_T_7, _wireSelectDC_T_12, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:24]
    node _commonSelt_T_6 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:51]
    node _commonSelt_T_7 = bits(_commonSelt_T_6, 0, 0) @[SDC_SDF_combFFT.scala 337:61]
    node _commonSelt_T_8 = eq(_commonSelt_T_7, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 337:44]
    node _commonSelt_T_9 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:71]
    node _commonSelt_T_10 = bits(_commonSelt_T_9, 0, 0) @[SDC_SDF_combFFT.scala 337:81]
    node _commonSelt_T_11 = mux(UInt<1>("h0"), _commonSelt_T_8, _commonSelt_T_10) @[SDC_SDF_combFFT.scala 337:22]
    reg stageIntf1_2_REG : SInt<32>, clock with :
      reset => (UInt<1>("h0"), stageIntf1_2_REG) @[SDC_SDF_combFFT.scala 344:33]
    node _T_9 = sub(dCount, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 321:42]
    node _T_10 = tail(_T_9, 1) @[SDC_SDF_combFFT.scala 321:42]
    node _T_11 = bits(_T_10, 4, 0)
    node _wireWRe_T_8 = geq(dCount, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 323:27]
    node _wireWRe_T_9 = lt(dCount, UInt<6>("h3f")) @[SDC_SDF_combFFT.scala 323:44]
    node _wireWRe_T_10 = and(_wireWRe_T_8, _wireWRe_T_9) @[SDC_SDF_combFFT.scala 323:34]
    node _WIRE_2_0 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_129 = validif(eq(UInt<1>("h0"), _T_11), _WIRE_2_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_1 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_130 = mux(eq(UInt<1>("h1"), _T_11), _WIRE_2_1, _GEN_129) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_2 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_131 = mux(eq(UInt<2>("h2"), _T_11), _WIRE_2_2, _GEN_130) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_3 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_132 = mux(eq(UInt<2>("h3"), _T_11), _WIRE_2_3, _GEN_131) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_4 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_133 = mux(eq(UInt<3>("h4"), _T_11), _WIRE_2_4, _GEN_132) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_5 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_134 = mux(eq(UInt<3>("h5"), _T_11), _WIRE_2_5, _GEN_133) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_6 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_135 = mux(eq(UInt<3>("h6"), _T_11), _WIRE_2_6, _GEN_134) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_7 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_136 = mux(eq(UInt<3>("h7"), _T_11), _WIRE_2_7, _GEN_135) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_8 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_137 = mux(eq(UInt<4>("h8"), _T_11), _WIRE_2_8, _GEN_136) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_9 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_138 = mux(eq(UInt<4>("h9"), _T_11), _WIRE_2_9, _GEN_137) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_10 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_139 = mux(eq(UInt<4>("ha"), _T_11), _WIRE_2_10, _GEN_138) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_11 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_140 = mux(eq(UInt<4>("hb"), _T_11), _WIRE_2_11, _GEN_139) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_12 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_141 = mux(eq(UInt<4>("hc"), _T_11), _WIRE_2_12, _GEN_140) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_13 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_142 = mux(eq(UInt<4>("hd"), _T_11), _WIRE_2_13, _GEN_141) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_14 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_143 = mux(eq(UInt<4>("he"), _T_11), _WIRE_2_14, _GEN_142) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_15 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_144 = mux(eq(UInt<4>("hf"), _T_11), _WIRE_2_15, _GEN_143) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_16 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_145 = mux(eq(UInt<5>("h10"), _T_11), _WIRE_2_16, _GEN_144) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_17 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_146 = mux(eq(UInt<5>("h11"), _T_11), _WIRE_2_17, _GEN_145) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_18 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_147 = mux(eq(UInt<5>("h12"), _T_11), _WIRE_2_18, _GEN_146) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_19 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_148 = mux(eq(UInt<5>("h13"), _T_11), _WIRE_2_19, _GEN_147) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_20 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_149 = mux(eq(UInt<5>("h14"), _T_11), _WIRE_2_20, _GEN_148) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_21 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_150 = mux(eq(UInt<5>("h15"), _T_11), _WIRE_2_21, _GEN_149) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_22 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_151 = mux(eq(UInt<5>("h16"), _T_11), _WIRE_2_22, _GEN_150) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_23 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_152 = mux(eq(UInt<5>("h17"), _T_11), _WIRE_2_23, _GEN_151) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_24 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_153 = mux(eq(UInt<5>("h18"), _T_11), _WIRE_2_24, _GEN_152) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_25 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_154 = mux(eq(UInt<5>("h19"), _T_11), _WIRE_2_25, _GEN_153) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_26 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_155 = mux(eq(UInt<5>("h1a"), _T_11), _WIRE_2_26, _GEN_154) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_27 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_156 = mux(eq(UInt<5>("h1b"), _T_11), _WIRE_2_27, _GEN_155) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_28 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_157 = mux(eq(UInt<5>("h1c"), _T_11), _WIRE_2_28, _GEN_156) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_29 = pad(UInt<3>("h4"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_158 = mux(eq(UInt<5>("h1d"), _T_11), _WIRE_2_29, _GEN_157) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_30 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_159 = mux(eq(UInt<5>("h1e"), _T_11), _WIRE_2_30, _GEN_158) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_31 = UInt<4>("hc") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_160 = mux(eq(UInt<5>("h1f"), _T_11), _WIRE_2_31, _GEN_159) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_2_T_11 = _GEN_160 @[SDC_SDF_combFFT.scala 323:19]
    node re_s_2_0 = asSInt(UInt<32>("h10000")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_161 = validif(eq(UInt<1>("h0"), _WIRE_2_T_11), re_s_2_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_1 = asSInt(UInt<32>("hfb15")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_162 = mux(eq(UInt<1>("h1"), _WIRE_2_T_11), re_s_2_1, _GEN_161) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_2 = asSInt(UInt<32>("hec83")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_163 = mux(eq(UInt<2>("h2"), _WIRE_2_T_11), re_s_2_2, _GEN_162) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_3 = asSInt(UInt<32>("hd4db")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_164 = mux(eq(UInt<2>("h3"), _WIRE_2_T_11), re_s_2_3, _GEN_163) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_4 = asSInt(UInt<32>("hb505")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_165 = mux(eq(UInt<3>("h4"), _WIRE_2_T_11), re_s_2_4, _GEN_164) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_5 = asSInt(UInt<32>("h8e3a")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_166 = mux(eq(UInt<3>("h5"), _WIRE_2_T_11), re_s_2_5, _GEN_165) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_6 = asSInt(UInt<32>("h61f8")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_167 = mux(eq(UInt<3>("h6"), _WIRE_2_T_11), re_s_2_6, _GEN_166) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_7 = asSInt(UInt<32>("h31f1")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_168 = mux(eq(UInt<3>("h7"), _WIRE_2_T_11), re_s_2_7, _GEN_167) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_8 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_169 = mux(eq(UInt<4>("h8"), _WIRE_2_T_11), re_s_2_8, _GEN_168) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_9 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_170 = mux(eq(UInt<4>("h9"), _WIRE_2_T_11), re_s_2_9, _GEN_169) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_10 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_171 = mux(eq(UInt<4>("ha"), _WIRE_2_T_11), re_s_2_10, _GEN_170) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_11 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_172 = mux(eq(UInt<4>("hb"), _WIRE_2_T_11), re_s_2_11, _GEN_171) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_173 = mux(eq(UInt<4>("hc"), _WIRE_2_T_11), re_s_2_12, _GEN_172) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_13 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_174 = mux(eq(UInt<4>("hd"), _WIRE_2_T_11), re_s_2_13, _GEN_173) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_14 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_175 = mux(eq(UInt<4>("he"), _WIRE_2_T_11), re_s_2_14, _GEN_174) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_2_15 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_176 = mux(eq(UInt<4>("hf"), _WIRE_2_T_11), re_s_2_15, _GEN_175) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _re_s_2_WIRE_2_T_11 = _GEN_176 @[SDC_SDF_combFFT.scala 323:19]
    node _wireWRe_T_11 = mux(_wireWRe_T_10, _re_s_2_WIRE_2_T_11, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 323:19]
    node _wireWIm_T_8 = geq(dCount, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 324:27]
    node _wireWIm_T_9 = lt(dCount, UInt<6>("h3f")) @[SDC_SDF_combFFT.scala 324:44]
    node _wireWIm_T_10 = and(_wireWIm_T_8, _wireWIm_T_9) @[SDC_SDF_combFFT.scala 324:34]
    node _WIRE_2_T_11_0 = _GEN_160 @[SDC_SDF_combFFT.scala 324:19]
    node im_s_2_0 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_177 = validif(eq(UInt<1>("h0"), _WIRE_2_T_11_0), im_s_2_0) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_1 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_178 = mux(eq(UInt<1>("h1"), _WIRE_2_T_11_0), im_s_2_1, _GEN_177) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_2 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_179 = mux(eq(UInt<2>("h2"), _WIRE_2_T_11_0), im_s_2_2, _GEN_178) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_3 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_180 = mux(eq(UInt<2>("h3"), _WIRE_2_T_11_0), im_s_2_3, _GEN_179) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_4 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_181 = mux(eq(UInt<3>("h4"), _WIRE_2_T_11_0), im_s_2_4, _GEN_180) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_5 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_182 = mux(eq(UInt<3>("h5"), _WIRE_2_T_11_0), im_s_2_5, _GEN_181) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_6 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_183 = mux(eq(UInt<3>("h6"), _WIRE_2_T_11_0), im_s_2_6, _GEN_182) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_7 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_184 = mux(eq(UInt<3>("h7"), _WIRE_2_T_11_0), im_s_2_7, _GEN_183) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_8 = asSInt(UInt<32>("hffff0000")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_185 = mux(eq(UInt<4>("h8"), _WIRE_2_T_11_0), im_s_2_8, _GEN_184) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_9 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_186 = mux(eq(UInt<4>("h9"), _WIRE_2_T_11_0), im_s_2_9, _GEN_185) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_10 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_187 = mux(eq(UInt<4>("ha"), _WIRE_2_T_11_0), im_s_2_10, _GEN_186) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_11 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_188 = mux(eq(UInt<4>("hb"), _WIRE_2_T_11_0), im_s_2_11, _GEN_187) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_189 = mux(eq(UInt<4>("hc"), _WIRE_2_T_11_0), im_s_2_12, _GEN_188) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_13 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_190 = mux(eq(UInt<4>("hd"), _WIRE_2_T_11_0), im_s_2_13, _GEN_189) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_14 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_191 = mux(eq(UInt<4>("he"), _WIRE_2_T_11_0), im_s_2_14, _GEN_190) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_2_15 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_192 = mux(eq(UInt<4>("hf"), _WIRE_2_T_11_0), im_s_2_15, _GEN_191) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node _im_s_2_WIRE_2_T_11_0 = _GEN_192 @[SDC_SDF_combFFT.scala 324:19]
    node _wireWIm_T_11 = mux(_wireWIm_T_10, _im_s_2_WIRE_2_T_11_0, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 324:19]
    node _wireSelectDC_T_14 = geq(dCount, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 329:32]
    node _wireSelectDC_T_15 = sub(dCount, UInt<5>("h1f")) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_16 = tail(_wireSelectDC_T_15, 1) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_17 = bits(_wireSelectDC_T_16, 2, 2) @[SDC_SDF_combFFT.scala 329:53]
    node _wireSelectDC_T_18 = bits(_wireSelectDC_T_17, 0, 0) @[SDC_SDF_combFFT.scala 329:77]
    node _wireSelectDC_T_19 = eq(_wireSelectDC_T_18, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:40]
    node _wireSelectDC_T_20 = mux(_wireSelectDC_T_14, _wireSelectDC_T_19, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:24]
    node _commonSelt_T_12 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:51]
    node _commonSelt_T_13 = bits(_commonSelt_T_12, 0, 0) @[SDC_SDF_combFFT.scala 337:61]
    node _commonSelt_T_14 = eq(_commonSelt_T_13, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 337:44]
    node _commonSelt_T_15 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:71]
    node _commonSelt_T_16 = bits(_commonSelt_T_15, 0, 0) @[SDC_SDF_combFFT.scala 337:81]
    node _commonSelt_T_17 = mux(UInt<1>("h1"), _commonSelt_T_14, _commonSelt_T_16) @[SDC_SDF_combFFT.scala 337:22]
    reg stageIntf1_3_REG : SInt<32>, clock with :
      reset => (UInt<1>("h0"), stageIntf1_3_REG) @[SDC_SDF_combFFT.scala 344:33]
    node _T_12 = sub(dCount, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 321:42]
    node _T_13 = tail(_T_12, 1) @[SDC_SDF_combFFT.scala 321:42]
    node _T_14 = bits(_T_13, 4, 0)
    node _wireWRe_T_12 = geq(dCount, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 323:27]
    node _wireWRe_T_13 = lt(dCount, UInt<7>("h42")) @[SDC_SDF_combFFT.scala 323:44]
    node _wireWRe_T_14 = and(_wireWRe_T_12, _wireWRe_T_13) @[SDC_SDF_combFFT.scala 323:34]
    node _WIRE_3_0 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_193 = validif(eq(UInt<1>("h0"), _T_14), _WIRE_3_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_1 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_194 = mux(eq(UInt<1>("h1"), _T_14), _WIRE_3_1, _GEN_193) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_2 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_195 = mux(eq(UInt<2>("h2"), _T_14), _WIRE_3_2, _GEN_194) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_3 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_196 = mux(eq(UInt<2>("h3"), _T_14), _WIRE_3_3, _GEN_195) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_4 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_197 = mux(eq(UInt<3>("h4"), _T_14), _WIRE_3_4, _GEN_196) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_5 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_198 = mux(eq(UInt<3>("h5"), _T_14), _WIRE_3_5, _GEN_197) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_6 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_199 = mux(eq(UInt<3>("h6"), _T_14), _WIRE_3_6, _GEN_198) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_7 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_200 = mux(eq(UInt<3>("h7"), _T_14), _WIRE_3_7, _GEN_199) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_8 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_201 = mux(eq(UInt<4>("h8"), _T_14), _WIRE_3_8, _GEN_200) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_9 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_202 = mux(eq(UInt<4>("h9"), _T_14), _WIRE_3_9, _GEN_201) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_10 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_203 = mux(eq(UInt<4>("ha"), _T_14), _WIRE_3_10, _GEN_202) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_11 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_204 = mux(eq(UInt<4>("hb"), _T_14), _WIRE_3_11, _GEN_203) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_12 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_205 = mux(eq(UInt<4>("hc"), _T_14), _WIRE_3_12, _GEN_204) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_13 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_206 = mux(eq(UInt<4>("hd"), _T_14), _WIRE_3_13, _GEN_205) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_14 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_207 = mux(eq(UInt<4>("he"), _T_14), _WIRE_3_14, _GEN_206) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_15 = pad(UInt<1>("h0"), 4) @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_208 = mux(eq(UInt<4>("hf"), _T_14), _WIRE_3_15, _GEN_207) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_16 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_209 = mux(eq(UInt<5>("h10"), _T_14), _WIRE_3_16, _GEN_208) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_17 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_210 = mux(eq(UInt<5>("h11"), _T_14), _WIRE_3_17, _GEN_209) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_18 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_211 = mux(eq(UInt<5>("h12"), _T_14), _WIRE_3_18, _GEN_210) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_19 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_212 = mux(eq(UInt<5>("h13"), _T_14), _WIRE_3_19, _GEN_211) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_20 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_213 = mux(eq(UInt<5>("h14"), _T_14), _WIRE_3_20, _GEN_212) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_21 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_214 = mux(eq(UInt<5>("h15"), _T_14), _WIRE_3_21, _GEN_213) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_22 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_215 = mux(eq(UInt<5>("h16"), _T_14), _WIRE_3_22, _GEN_214) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_23 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_216 = mux(eq(UInt<5>("h17"), _T_14), _WIRE_3_23, _GEN_215) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_24 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_217 = mux(eq(UInt<5>("h18"), _T_14), _WIRE_3_24, _GEN_216) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_25 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_218 = mux(eq(UInt<5>("h19"), _T_14), _WIRE_3_25, _GEN_217) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_26 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_219 = mux(eq(UInt<5>("h1a"), _T_14), _WIRE_3_26, _GEN_218) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_27 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_220 = mux(eq(UInt<5>("h1b"), _T_14), _WIRE_3_27, _GEN_219) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_28 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_221 = mux(eq(UInt<5>("h1c"), _T_14), _WIRE_3_28, _GEN_220) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_29 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_222 = mux(eq(UInt<5>("h1d"), _T_14), _WIRE_3_29, _GEN_221) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_30 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_223 = mux(eq(UInt<5>("h1e"), _T_14), _WIRE_3_30, _GEN_222) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_31 = UInt<4>("h8") @[SDC_SDF_combFFT.scala 300:12 SDC_SDF_combFFT.scala 300:12]
    node _GEN_224 = mux(eq(UInt<5>("h1f"), _T_14), _WIRE_3_31, _GEN_223) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _WIRE_3_T_14 = _GEN_224 @[SDC_SDF_combFFT.scala 323:19]
    node re_s_3_0 = asSInt(UInt<32>("h10000")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_225 = validif(eq(UInt<1>("h0"), _WIRE_3_T_14), re_s_3_0) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_1 = asSInt(UInt<32>("hfb15")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_226 = mux(eq(UInt<1>("h1"), _WIRE_3_T_14), re_s_3_1, _GEN_225) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_2 = asSInt(UInt<32>("hec83")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_227 = mux(eq(UInt<2>("h2"), _WIRE_3_T_14), re_s_3_2, _GEN_226) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_3 = asSInt(UInt<32>("hd4db")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_228 = mux(eq(UInt<2>("h3"), _WIRE_3_T_14), re_s_3_3, _GEN_227) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_4 = asSInt(UInt<32>("hb505")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_229 = mux(eq(UInt<3>("h4"), _WIRE_3_T_14), re_s_3_4, _GEN_228) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_5 = asSInt(UInt<32>("h8e3a")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_230 = mux(eq(UInt<3>("h5"), _WIRE_3_T_14), re_s_3_5, _GEN_229) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_6 = asSInt(UInt<32>("h61f8")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_231 = mux(eq(UInt<3>("h6"), _WIRE_3_T_14), re_s_3_6, _GEN_230) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_7 = asSInt(UInt<32>("h31f1")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_232 = mux(eq(UInt<3>("h7"), _WIRE_3_T_14), re_s_3_7, _GEN_231) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_8 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_233 = mux(eq(UInt<4>("h8"), _WIRE_3_T_14), re_s_3_8, _GEN_232) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_9 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_234 = mux(eq(UInt<4>("h9"), _WIRE_3_T_14), re_s_3_9, _GEN_233) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_10 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_235 = mux(eq(UInt<4>("ha"), _WIRE_3_T_14), re_s_3_10, _GEN_234) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_11 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_236 = mux(eq(UInt<4>("hb"), _WIRE_3_T_14), re_s_3_11, _GEN_235) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_237 = mux(eq(UInt<4>("hc"), _WIRE_3_T_14), re_s_3_12, _GEN_236) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_13 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_238 = mux(eq(UInt<4>("hd"), _WIRE_3_T_14), re_s_3_13, _GEN_237) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_14 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_239 = mux(eq(UInt<4>("he"), _WIRE_3_T_14), re_s_3_14, _GEN_238) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node re_s_3_15 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 282:23 SDC_SDF_combFFT.scala 282:23]
    node _GEN_240 = mux(eq(UInt<4>("hf"), _WIRE_3_T_14), re_s_3_15, _GEN_239) @[SDC_SDF_combFFT.scala 323:19 SDC_SDF_combFFT.scala 323:19]
    node _re_s_3_WIRE_3_T_14 = _GEN_240 @[SDC_SDF_combFFT.scala 323:19]
    node _wireWRe_T_15 = mux(_wireWRe_T_14, _re_s_3_WIRE_3_T_14, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 323:19]
    node _wireWIm_T_12 = geq(dCount, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 324:27]
    node _wireWIm_T_13 = lt(dCount, UInt<7>("h42")) @[SDC_SDF_combFFT.scala 324:44]
    node _wireWIm_T_14 = and(_wireWIm_T_12, _wireWIm_T_13) @[SDC_SDF_combFFT.scala 324:34]
    node _WIRE_3_T_14_0 = _GEN_224 @[SDC_SDF_combFFT.scala 324:19]
    node im_s_3_0 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_241 = validif(eq(UInt<1>("h0"), _WIRE_3_T_14_0), im_s_3_0) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_1 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_242 = mux(eq(UInt<1>("h1"), _WIRE_3_T_14_0), im_s_3_1, _GEN_241) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_2 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_243 = mux(eq(UInt<2>("h2"), _WIRE_3_T_14_0), im_s_3_2, _GEN_242) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_3 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_244 = mux(eq(UInt<2>("h3"), _WIRE_3_T_14_0), im_s_3_3, _GEN_243) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_4 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_245 = mux(eq(UInt<3>("h4"), _WIRE_3_T_14_0), im_s_3_4, _GEN_244) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_5 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_246 = mux(eq(UInt<3>("h5"), _WIRE_3_T_14_0), im_s_3_5, _GEN_245) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_6 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_247 = mux(eq(UInt<3>("h6"), _WIRE_3_T_14_0), im_s_3_6, _GEN_246) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_7 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_248 = mux(eq(UInt<3>("h7"), _WIRE_3_T_14_0), im_s_3_7, _GEN_247) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_8 = asSInt(UInt<32>("hffff0000")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_249 = mux(eq(UInt<4>("h8"), _WIRE_3_T_14_0), im_s_3_8, _GEN_248) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_9 = asSInt(UInt<32>("hffff04eb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_250 = mux(eq(UInt<4>("h9"), _WIRE_3_T_14_0), im_s_3_9, _GEN_249) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_10 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_251 = mux(eq(UInt<4>("ha"), _WIRE_3_T_14_0), im_s_3_10, _GEN_250) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_11 = asSInt(UInt<32>("hffff2b25")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_252 = mux(eq(UInt<4>("hb"), _WIRE_3_T_14_0), im_s_3_11, _GEN_251) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_12 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_253 = mux(eq(UInt<4>("hc"), _WIRE_3_T_14_0), im_s_3_12, _GEN_252) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_13 = asSInt(UInt<32>("hffff71c6")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_254 = mux(eq(UInt<4>("hd"), _WIRE_3_T_14_0), im_s_3_13, _GEN_253) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_14 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_255 = mux(eq(UInt<4>("he"), _WIRE_3_T_14_0), im_s_3_14, _GEN_254) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node im_s_3_15 = asSInt(UInt<32>("hffffce0f")) @[SDC_SDF_combFFT.scala 283:23 SDC_SDF_combFFT.scala 283:23]
    node _GEN_256 = mux(eq(UInt<4>("hf"), _WIRE_3_T_14_0), im_s_3_15, _GEN_255) @[SDC_SDF_combFFT.scala 324:19 SDC_SDF_combFFT.scala 324:19]
    node _im_s_3_WIRE_3_T_14_0 = _GEN_256 @[SDC_SDF_combFFT.scala 324:19]
    node _wireWIm_T_15 = mux(_wireWIm_T_14, _im_s_3_WIRE_3_T_14_0, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 324:19]
    node _wireSelectDC_T_21 = geq(dCount, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 329:32]
    node _wireSelectDC_T_22 = sub(dCount, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_23 = tail(_wireSelectDC_T_22, 1) @[SDC_SDF_combFFT.scala 329:48]
    node _wireSelectDC_T_24 = bits(_wireSelectDC_T_23, 1, 1) @[SDC_SDF_combFFT.scala 329:53]
    node _wireSelectDC_T_25 = bits(_wireSelectDC_T_24, 0, 0) @[SDC_SDF_combFFT.scala 329:77]
    node _wireSelectDC_T_26 = eq(_wireSelectDC_T_25, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:40]
    node _wireSelectDC_T_27 = mux(_wireSelectDC_T_21, _wireSelectDC_T_26, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 329:24]
    node _commonSelt_T_18 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:51]
    node _commonSelt_T_19 = bits(_commonSelt_T_18, 0, 0) @[SDC_SDF_combFFT.scala 337:61]
    node _commonSelt_T_20 = eq(_commonSelt_T_19, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 337:44]
    node _commonSelt_T_21 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 337:71]
    node _commonSelt_T_22 = bits(_commonSelt_T_21, 0, 0) @[SDC_SDF_combFFT.scala 337:81]
    node _commonSelt_T_23 = mux(UInt<1>("h0"), _commonSelt_T_20, _commonSelt_T_22) @[SDC_SDF_combFFT.scala 337:22]
    reg stageIntf1_4_REG : SInt<32>, clock with :
      reset => (UInt<1>("h0"), stageIntf1_4_REG) @[SDC_SDF_combFFT.scala 344:33]
    node _postStgSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 347:58]
    node _postStgSelt_T_1 = bits(_postStgSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 347:68]
    node _postStgSelt_T_2 = eq(_postStgSelt_T_1, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 347:51]
    node _postStgSelt_T_3 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 347:78]
    node _postStgSelt_T_4 = bits(_postStgSelt_T_3, 0, 0) @[SDC_SDF_combFFT.scala 347:88]
    node postStgSelt = mux(UInt<1>("h1"), _postStgSelt_T_2, _postStgSelt_T_4) @[SDC_SDF_combFFT.scala 347:24]
    node _wireSDFAdd_T = geq(dCount, UInt<6>("h24")) @[SDC_SDF_combFFT.scala 355:28]
    node _wireSDFAdd_T_1 = lt(dCount, UInt<7>("h44")) @[SDC_SDF_combFFT.scala 355:64]
    node _wireSDFAdd_T_2 = and(_wireSDFAdd_T, _wireSDFAdd_T_1) @[SDC_SDF_combFFT.scala 355:54]
    node _wireSDFAdd_T_3 = mux(_wireSDFAdd_T_2, UInt<1>("h0"), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 355:20]
    node _wireSDFSub_T = geq(dCount, UInt<6>("h34")) @[SDC_SDF_combFFT.scala 356:28]
    node _wireSDFSub_T_1 = lt(dCount, UInt<7>("h44")) @[SDC_SDF_combFFT.scala 356:74]
    node _wireSDFSub_T_2 = and(_wireSDFSub_T, _wireSDFSub_T_1) @[SDC_SDF_combFFT.scala 356:64]
    node _wireSDFSub_T_3 = mux(_wireSDFSub_T_2, UInt<1>("h0"), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 356:20]
    node _wireSDFRst_T = eq(dCount, UInt<7>("h53")) @[SDC_SDF_combFFT.scala 357:28]
    node _wireSDFRst_T_1 = mux(_wireSDFRst_T, UInt<1>("h1"), UInt<1>("h0")) @[SDC_SDF_combFFT.scala 357:20]
    reg reorderModule_io_inValid_REG : UInt<7>, clock with :
      reset => (UInt<1>("h0"), reorderModule_io_inValid_REG) @[SDC_SDF_combFFT.scala 362:38]
    node _reorderModule_io_inValid_T = eq(reorderModule_io_inValid_REG, UInt<6>("h33")) @[SDC_SDF_combFFT.scala 362:47]
    node stageIntf1_0 = inst.io_out1 @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 314:17]
    node stageIntf1_1 = stageIntf1_1_REG @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 344:23]
    node stageIntf1_2 = stageIntf1_2_REG @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 344:23]
    node stageIntf1_3 = stageIntf1_3_REG @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 344:23]
    node stageIntf1_4 = stageIntf1_4_REG @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 344:23]
    node stageIntf2_0 = inst.io_out2 @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 315:17]
    node stageIntf2_1 = stageIntf2_1_inst.io_out @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 343:23]
    node stageIntf2_2 = stageIntf2_2_inst.io_out @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 343:23]
    node stageIntf2_3 = stageIntf2_3_inst.io_out @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 343:23]
    node stageIntf2_4 = stageIntf2_4_inst.io_out @[SDC_SDF_combFFT.scala 311:39 SDC_SDF_combFFT.scala 343:23]
    node wireWRe = _wireWRe_T_3 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 323:13]
    node wireWIm = _wireWIm_T_3 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 324:13]
    node wireSelectDC = _wireSelectDC_T_6 @[SDC_SDF_combFFT.scala 328:28 SDC_SDF_combFFT.scala 329:18]
    node commonSelt = _commonSelt_T_5 @[SDC_SDF_combFFT.scala 336:26 SDC_SDF_combFFT.scala 337:16]
    node wireWRe_1 = _wireWRe_T_7 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 323:13]
    node wireWIm_1 = _wireWIm_T_7 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 324:13]
    node wireSelectDC_1 = _wireSelectDC_T_13 @[SDC_SDF_combFFT.scala 328:28 SDC_SDF_combFFT.scala 329:18]
    node commonSelt_1 = _commonSelt_T_11 @[SDC_SDF_combFFT.scala 336:26 SDC_SDF_combFFT.scala 337:16]
    node wireWRe_2 = _wireWRe_T_11 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 323:13]
    node wireWIm_2 = _wireWIm_T_11 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 324:13]
    node wireSelectDC_2 = _wireSelectDC_T_20 @[SDC_SDF_combFFT.scala 328:28 SDC_SDF_combFFT.scala 329:18]
    node commonSelt_2 = _commonSelt_T_17 @[SDC_SDF_combFFT.scala 336:26 SDC_SDF_combFFT.scala 337:16]
    node wireWRe_3 = _wireWRe_T_15 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 323:13]
    node wireWIm_3 = _wireWIm_T_15 @[SDC_SDF_combFFT.scala 322:32 SDC_SDF_combFFT.scala 324:13]
    node wireSelectDC_3 = _wireSelectDC_T_27 @[SDC_SDF_combFFT.scala 328:28 SDC_SDF_combFFT.scala 329:18]
    node commonSelt_3 = _commonSelt_T_23 @[SDC_SDF_combFFT.scala 336:26 SDC_SDF_combFFT.scala 337:16]
    node wireFpToComp_re = inst_21.io_out1 @[SDC_SDF_combFFT.scala 350:26 SDC_SDF_combFFT.scala 351:19]
    node wireFpToComp_im = inst_21.io_out2 @[SDC_SDF_combFFT.scala 350:26 SDC_SDF_combFFT.scala 352:19]
    node wireSDFAdd = _wireSDFAdd_T_3 @[SDC_SDF_combFFT.scala 354:48 SDC_SDF_combFFT.scala 355:14]
    node wireSDFSub = _wireSDFSub_T_3 @[SDC_SDF_combFFT.scala 354:48 SDC_SDF_combFFT.scala 356:14]
    node wireSDFRst = _wireSDFRst_T_1 @[SDC_SDF_combFFT.scala 354:48 SDC_SDF_combFFT.scala 357:14]
    io_dataOut_re <= reorderModule.io_out_re @[SDC_SDF_combFFT.scala 363:14]
    io_dataOut_im <= reorderModule.io_out_im @[SDC_SDF_combFFT.scala 363:14]
    io_outValid <= reorderModule.io_outValid @[SDC_SDF_combFFT.scala 364:15]
    io_busy <= busy @[SDC_SDF_combFFT.scala 309:11]
    dCount <= mux(reset, UInt<7>("h0"), _GEN_0) @[SDC_SDF_combFFT.scala 304:23 SDC_SDF_combFFT.scala 304:23]
    inst.clock <= clock
    inst.reset <= reset
    inst.io_in1 <= io_dataIn_re @[SDC_SDF_combFFT.scala 87:17]
    inst.io_in2 <= io_dataIn_im @[SDC_SDF_combFFT.scala 88:17]
    inst.io_s <= _T_2 @[SDC_SDF_combFFT.scala 89:15]
    inst_1.clock <= clock
    inst_1.reset <= reset
    inst_1.io_in1 <= stageIntf1_0 @[SDC_SDF_combFFT.scala 87:17]
    inst_1.io_in2 <= stageIntf2_0 @[SDC_SDF_combFFT.scala 88:17]
    inst_1.io_s <= wireSelectDC @[SDC_SDF_combFFT.scala 89:15]
    inst_2.clock <= clock
    inst_2.reset <= reset
    inst_2.io_a <= inst_1.io_out1 @[SDC_SDF_combFFT.scala 145:15]
    inst_2.io_b <= inst_1.io_out2 @[SDC_SDF_combFFT.scala 146:15]
    inst_3.clock <= clock
    inst_3.reset <= reset
    inst_3.io_dataIn <= inst_2.io_B @[SDC_SDF_combFFT.scala 199:20]
    inst_3.io_w_re_in <= wireWRe @[SDC_SDF_combFFT.scala 200:21]
    inst_3.io_w_im_in <= wireWIm @[SDC_SDF_combFFT.scala 201:21]
    inst_4.clock <= clock
    inst_4.reset <= reset
    inst_4.io_s <= commonSelt @[SDC_SDF_combFFT.scala 177:15]
    inst_4.io_in1 <= inst_3.io_out1 @[SDC_SDF_combFFT.scala 175:17]
    inst_4.io_in2 <= inst_3.io_out2 @[SDC_SDF_combFFT.scala 176:17]
    inst_5.clock <= clock
    inst_5.reset <= reset
    inst_5.io_in1 <= inst_4.io_out1 @[SDC_SDF_combFFT.scala 87:17]
    inst_5.io_in2 <= inst_4.io_out2 @[SDC_SDF_combFFT.scala 88:17]
    inst_5.io_s <= commonSelt @[SDC_SDF_combFFT.scala 89:15]
    stageIntf2_1_inst.clock <= clock
    stageIntf2_1_inst.reset <= reset
    stageIntf2_1_inst.io_in1 <= inst_5.io_out1 @[SDC_SDF_combFFT.scala 227:17]
    stageIntf2_1_inst.io_in2 <= inst_5.io_out2 @[SDC_SDF_combFFT.scala 228:17]
    stageIntf2_1_inst.io_s <= commonSelt @[SDC_SDF_combFFT.scala 229:15]
    stageIntf1_1_REG <= inst_2.io_A @[SDC_SDF_combFFT.scala 344:33]
    inst_6.clock <= clock
    inst_6.reset <= reset
    inst_6.io_in1 <= stageIntf1_1 @[SDC_SDF_combFFT.scala 87:17]
    inst_6.io_in2 <= stageIntf2_1 @[SDC_SDF_combFFT.scala 88:17]
    inst_6.io_s <= wireSelectDC_1 @[SDC_SDF_combFFT.scala 89:15]
    inst_7.clock <= clock
    inst_7.reset <= reset
    inst_7.io_a <= inst_6.io_out1 @[SDC_SDF_combFFT.scala 145:15]
    inst_7.io_b <= inst_6.io_out2 @[SDC_SDF_combFFT.scala 146:15]
    inst_8.clock <= clock
    inst_8.reset <= reset
    inst_8.io_dataIn <= inst_7.io_B @[SDC_SDF_combFFT.scala 199:20]
    inst_8.io_w_re_in <= wireWRe_1 @[SDC_SDF_combFFT.scala 200:21]
    inst_8.io_w_im_in <= wireWIm_1 @[SDC_SDF_combFFT.scala 201:21]
    inst_9.clock <= clock
    inst_9.reset <= reset
    inst_9.io_s <= commonSelt_1 @[SDC_SDF_combFFT.scala 177:15]
    inst_9.io_in1 <= inst_8.io_out1 @[SDC_SDF_combFFT.scala 175:17]
    inst_9.io_in2 <= inst_8.io_out2 @[SDC_SDF_combFFT.scala 176:17]
    inst_10.clock <= clock
    inst_10.reset <= reset
    inst_10.io_in1 <= inst_9.io_out1 @[SDC_SDF_combFFT.scala 87:17]
    inst_10.io_in2 <= inst_9.io_out2 @[SDC_SDF_combFFT.scala 88:17]
    inst_10.io_s <= commonSelt_1 @[SDC_SDF_combFFT.scala 89:15]
    stageIntf2_2_inst.clock <= clock
    stageIntf2_2_inst.reset <= reset
    stageIntf2_2_inst.io_in1 <= inst_10.io_out1 @[SDC_SDF_combFFT.scala 227:17]
    stageIntf2_2_inst.io_in2 <= inst_10.io_out2 @[SDC_SDF_combFFT.scala 228:17]
    stageIntf2_2_inst.io_s <= commonSelt_1 @[SDC_SDF_combFFT.scala 229:15]
    stageIntf1_2_REG <= inst_7.io_A @[SDC_SDF_combFFT.scala 344:33]
    inst_11.clock <= clock
    inst_11.reset <= reset
    inst_11.io_in1 <= stageIntf1_2 @[SDC_SDF_combFFT.scala 87:17]
    inst_11.io_in2 <= stageIntf2_2 @[SDC_SDF_combFFT.scala 88:17]
    inst_11.io_s <= wireSelectDC_2 @[SDC_SDF_combFFT.scala 89:15]
    inst_12.clock <= clock
    inst_12.reset <= reset
    inst_12.io_a <= inst_11.io_out1 @[SDC_SDF_combFFT.scala 145:15]
    inst_12.io_b <= inst_11.io_out2 @[SDC_SDF_combFFT.scala 146:15]
    inst_13.clock <= clock
    inst_13.reset <= reset
    inst_13.io_dataIn <= inst_12.io_B @[SDC_SDF_combFFT.scala 199:20]
    inst_13.io_w_re_in <= wireWRe_2 @[SDC_SDF_combFFT.scala 200:21]
    inst_13.io_w_im_in <= wireWIm_2 @[SDC_SDF_combFFT.scala 201:21]
    inst_14.clock <= clock
    inst_14.reset <= reset
    inst_14.io_s <= commonSelt_2 @[SDC_SDF_combFFT.scala 177:15]
    inst_14.io_in1 <= inst_13.io_out1 @[SDC_SDF_combFFT.scala 175:17]
    inst_14.io_in2 <= inst_13.io_out2 @[SDC_SDF_combFFT.scala 176:17]
    inst_15.clock <= clock
    inst_15.reset <= reset
    inst_15.io_in1 <= inst_14.io_out1 @[SDC_SDF_combFFT.scala 87:17]
    inst_15.io_in2 <= inst_14.io_out2 @[SDC_SDF_combFFT.scala 88:17]
    inst_15.io_s <= commonSelt_2 @[SDC_SDF_combFFT.scala 89:15]
    stageIntf2_3_inst.clock <= clock
    stageIntf2_3_inst.reset <= reset
    stageIntf2_3_inst.io_in1 <= inst_15.io_out1 @[SDC_SDF_combFFT.scala 227:17]
    stageIntf2_3_inst.io_in2 <= inst_15.io_out2 @[SDC_SDF_combFFT.scala 228:17]
    stageIntf2_3_inst.io_s <= commonSelt_2 @[SDC_SDF_combFFT.scala 229:15]
    stageIntf1_3_REG <= inst_12.io_A @[SDC_SDF_combFFT.scala 344:33]
    inst_16.clock <= clock
    inst_16.reset <= reset
    inst_16.io_in1 <= stageIntf1_3 @[SDC_SDF_combFFT.scala 87:17]
    inst_16.io_in2 <= stageIntf2_3 @[SDC_SDF_combFFT.scala 88:17]
    inst_16.io_s <= wireSelectDC_3 @[SDC_SDF_combFFT.scala 89:15]
    inst_17.clock <= clock
    inst_17.reset <= reset
    inst_17.io_a <= inst_16.io_out1 @[SDC_SDF_combFFT.scala 145:15]
    inst_17.io_b <= inst_16.io_out2 @[SDC_SDF_combFFT.scala 146:15]
    inst_18.clock <= clock
    inst_18.reset <= reset
    inst_18.io_dataIn <= inst_17.io_B @[SDC_SDF_combFFT.scala 199:20]
    inst_18.io_w_re_in <= wireWRe_3 @[SDC_SDF_combFFT.scala 200:21]
    inst_18.io_w_im_in <= wireWIm_3 @[SDC_SDF_combFFT.scala 201:21]
    inst_19.clock <= clock
    inst_19.reset <= reset
    inst_19.io_s <= commonSelt_3 @[SDC_SDF_combFFT.scala 177:15]
    inst_19.io_in1 <= inst_18.io_out1 @[SDC_SDF_combFFT.scala 175:17]
    inst_19.io_in2 <= inst_18.io_out2 @[SDC_SDF_combFFT.scala 176:17]
    inst_20.clock <= clock
    inst_20.reset <= reset
    inst_20.io_in1 <= inst_19.io_out1 @[SDC_SDF_combFFT.scala 87:17]
    inst_20.io_in2 <= inst_19.io_out2 @[SDC_SDF_combFFT.scala 88:17]
    inst_20.io_s <= commonSelt_3 @[SDC_SDF_combFFT.scala 89:15]
    stageIntf2_4_inst.clock <= clock
    stageIntf2_4_inst.reset <= reset
    stageIntf2_4_inst.io_in1 <= inst_20.io_out1 @[SDC_SDF_combFFT.scala 227:17]
    stageIntf2_4_inst.io_in2 <= inst_20.io_out2 @[SDC_SDF_combFFT.scala 228:17]
    stageIntf2_4_inst.io_s <= commonSelt_3 @[SDC_SDF_combFFT.scala 229:15]
    stageIntf1_4_REG <= inst_17.io_A @[SDC_SDF_combFFT.scala 344:33]
    inst_21.clock <= clock
    inst_21.reset <= reset
    inst_21.io_in1 <= stageIntf1_4 @[SDC_SDF_combFFT.scala 87:17]
    inst_21.io_in2 <= stageIntf2_4 @[SDC_SDF_combFFT.scala 88:17]
    inst_21.io_s <= postStgSelt @[SDC_SDF_combFFT.scala 89:15]
    sdfReturn_inst.clock <= clock
    sdfReturn_inst.reset <= reset
    sdfReturn_inst.io_in_re <= wireFpToComp_re @[SDC_SDF_combFFT.scala 121:16]
    sdfReturn_inst.io_in_im <= wireFpToComp_im @[SDC_SDF_combFFT.scala 121:16]
    sdfReturn_inst.io_ctrlSub <= wireSDFSub @[SDC_SDF_combFFT.scala 122:21]
    sdfReturn_inst.io_ctrlAdd <= wireSDFAdd @[SDC_SDF_combFFT.scala 123:21]
    sdfReturn_inst.io_altRst <= wireSDFRst @[SDC_SDF_combFFT.scala 124:20]
    reorderModule.clock <= clock
    reorderModule.reset <= reset
    reorderModule.io_in_re <= sdfReturn_inst.io_out_re @[SDC_SDF_combFFT.scala 361:23]
    reorderModule.io_in_im <= sdfReturn_inst.io_out_im @[SDC_SDF_combFFT.scala 361:23]
    reorderModule.io_inValid <= _reorderModule_io_inValid_T @[SDC_SDF_combFFT.scala 362:28]
    reorderModule_io_inValid_REG <= dCount @[SDC_SDF_combFFT.scala 362:38]
