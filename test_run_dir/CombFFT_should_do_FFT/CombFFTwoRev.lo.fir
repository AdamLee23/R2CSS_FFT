circuit CombFFTwoRev :
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
    node wire1 = wire1_r @[SDC_SDF_combFFT.scala 79:19 SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 82:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 80:19 SDC_SDF_combFFT.scala 82:9]
    node _GEN_1 = mux(UInt<1>("h1"), wire2, io_out1_r) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io_out1 <= io_out1_r @[SDC_SDF_combFFT.scala 83:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
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
    node wire1 = wire1_r_7 @[SDC_SDF_combFFT.scala 79:19 SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io_s, wire1, io_in1) @[SDC_SDF_combFFT.scala 82:15]
    reg io_out1_r : SInt<32>, clock with :
      reset => (UInt<1>("h0"), io_out1_r) @[Reg.scala 15:16]
    node wire2 = _wire2_T @[SDC_SDF_combFFT.scala 80:19 SDC_SDF_combFFT.scala 82:9]
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
    node _io_out2_T = mux(io_s, io_in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io_out1 <= io_out1_r_7 @[SDC_SDF_combFFT.scala 83:11]
    io_out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
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

  module RealAddSub :
    input clock : Clock
    input reset : UInt<1>
    input io_a : SInt<32>
    input io_b : SInt<32>
    output io_A : SInt<32>
    output io_B : SInt<32>

    node _io_A_T = add(io_a, io_b) @[SDC_SDF_combFFT.scala 131:16]
    node _io_A_T_1 = tail(_io_A_T, 1) @[SDC_SDF_combFFT.scala 131:16]
    node _io_A_T_2 = asSInt(_io_A_T_1) @[SDC_SDF_combFFT.scala 131:16]
    node _io_B_T = sub(io_a, io_b) @[SDC_SDF_combFFT.scala 132:16]
    node _io_B_T_1 = tail(_io_B_T, 1) @[SDC_SDF_combFFT.scala 132:16]
    node _io_B_T_2 = asSInt(_io_B_T_1) @[SDC_SDF_combFFT.scala 132:16]
    io_A <= _io_A_T_2 @[SDC_SDF_combFFT.scala 131:8]
    io_B <= _io_B_T_2 @[SDC_SDF_combFFT.scala 132:8]

  module RealBranchMul :
    input clock : Clock
    input reset : UInt<1>
    input io_dataIn : SInt<32>
    input io_w_re_in : SInt<32>
    input io_w_im_in : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _io_out1_T = mul(io_w_re_in, io_dataIn) @[SDC_SDF_combFFT.scala 182:25]
    node _io_out2_T = mul(io_w_im_in, io_dataIn) @[SDC_SDF_combFFT.scala 183:25]
    io_out1 <= asSInt(bits(shr(_io_out1_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 182:11]
    io_out2 <= asSInt(bits(shr(_io_out2_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 183:11]

  module Switch :
    input clock : Clock
    input reset : UInt<1>
    input io_s : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _GEN_0 = mux(io_s, io_in1, io_in2) @[SDC_SDF_combFFT.scala 154:15 SDC_SDF_combFFT.scala 155:13 SDC_SDF_combFFT.scala 158:13]
    node _GEN_1 = mux(io_s, io_in2, io_in1) @[SDC_SDF_combFFT.scala 154:15 SDC_SDF_combFFT.scala 156:13 SDC_SDF_combFFT.scala 159:13]
    io_out1 <= _GEN_0
    io_out2 <= _GEN_1

  module RealAdder :
    input clock : Clock
    input reset : UInt<1>
    input io_in1 : SInt<32>
    input io_in2 : SInt<32>
    input io_s : UInt<1>
    output io_out : SInt<32>

    node _io_out_T = sub(io_in1, io_in2) @[SDC_SDF_combFFT.scala 205:22]
    node _io_out_T_1 = tail(_io_out_T, 1) @[SDC_SDF_combFFT.scala 205:22]
    node _io_out_T_2 = asSInt(_io_out_T_1) @[SDC_SDF_combFFT.scala 205:22]
    node _io_out_T_3 = add(io_in1, io_in2) @[SDC_SDF_combFFT.scala 207:22]
    node _io_out_T_4 = tail(_io_out_T_3, 1) @[SDC_SDF_combFFT.scala 207:22]
    node _io_out_T_5 = asSInt(_io_out_T_4) @[SDC_SDF_combFFT.scala 207:22]
    node _GEN_0 = mux(io_s, _io_out_T_2, _io_out_T_5) @[SDC_SDF_combFFT.scala 204:15 SDC_SDF_combFFT.scala 205:12 SDC_SDF_combFFT.scala 207:12]
    io_out <= _GEN_0

  module ComplexAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = add(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_im_T = add(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 41:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 41:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 41:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 40:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 41:13]

  module ComplexSub :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = sub(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_im_T = sub(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 58:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 58:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 58:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 57:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 58:13]

  module SDFUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    input io_ctrl : UInt<1>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>

    inst wire1_inst of ComplexAdd @[SDC_SDF_combFFT.scala 47:22]
    inst io_out_inst of ComplexSub @[SDC_SDF_combFFT.scala 64:22]
    reg wire2_r_7_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_re) @[Reg.scala 15:16]
    node wire2_re = wire2_r_7_re @[SDC_SDF_combFFT.scala 105:19 SDC_SDF_combFFT.scala 110:9]
    node _io_out_T_re = mux(io_ctrl, wire2_re, io_out_inst.io_res_re) @[SDC_SDF_combFFT.scala 109:16]
    reg wire2_r_7_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_im) @[Reg.scala 15:16]
    node wire2_im = wire2_r_7_im @[SDC_SDF_combFFT.scala 105:19 SDC_SDF_combFFT.scala 110:9]
    node _io_out_T_im = mux(io_ctrl, wire2_im, io_out_inst.io_res_im) @[SDC_SDF_combFFT.scala 109:16]
    reg wire2_r_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_re) @[Reg.scala 15:16]
    reg wire2_r_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_im) @[Reg.scala 15:16]
    node wire1_im = wire1_inst.io_res_im @[SDC_SDF_combFFT.scala 104:19 SDC_SDF_combFFT.scala 106:9]
    node _GEN_0 = mux(UInt<1>("h1"), wire1_im, wire2_r_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1_re = wire1_inst.io_res_re @[SDC_SDF_combFFT.scala 104:19 SDC_SDF_combFFT.scala 106:9]
    node _GEN_1 = mux(UInt<1>("h1"), wire1_re, wire2_r_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_1_re) @[Reg.scala 15:16]
    reg wire2_r_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_1_im) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), wire2_r_im, wire2_r_1_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_3 = mux(UInt<1>("h1"), wire2_r_re, wire2_r_1_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_2_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_2_re) @[Reg.scala 15:16]
    reg wire2_r_2_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_2_im) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), wire2_r_1_im, wire2_r_2_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_5 = mux(UInt<1>("h1"), wire2_r_1_re, wire2_r_2_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_3_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_3_re) @[Reg.scala 15:16]
    reg wire2_r_3_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_3_im) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), wire2_r_2_im, wire2_r_3_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_7 = mux(UInt<1>("h1"), wire2_r_2_re, wire2_r_3_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_4_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_4_re) @[Reg.scala 15:16]
    reg wire2_r_4_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_4_im) @[Reg.scala 15:16]
    node _GEN_8 = mux(UInt<1>("h1"), wire2_r_3_im, wire2_r_4_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_9 = mux(UInt<1>("h1"), wire2_r_3_re, wire2_r_4_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_5_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_5_re) @[Reg.scala 15:16]
    reg wire2_r_5_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_5_im) @[Reg.scala 15:16]
    node _GEN_10 = mux(UInt<1>("h1"), wire2_r_4_im, wire2_r_5_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_11 = mux(UInt<1>("h1"), wire2_r_4_re, wire2_r_5_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    reg wire2_r_6_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_6_re) @[Reg.scala 15:16]
    reg wire2_r_6_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_6_im) @[Reg.scala 15:16]
    node _GEN_12 = mux(UInt<1>("h1"), wire2_r_5_im, wire2_r_6_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_13 = mux(UInt<1>("h1"), wire2_r_5_re, wire2_r_6_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), wire2_r_6_im, wire2_r_7_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_15 = mux(UInt<1>("h1"), wire2_r_6_re, wire2_r_7_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    io_out_re <= _io_out_T_re @[SDC_SDF_combFFT.scala 109:10]
    io_out_im <= _io_out_T_im @[SDC_SDF_combFFT.scala 109:10]
    wire1_inst.clock <= clock
    wire1_inst.reset <= reset
    wire1_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 49:17]
    wire1_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 49:17]
    io_out_inst.clock <= clock
    io_out_inst.reset <= reset
    io_out_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 66:17]
    io_out_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 66:17]
    wire2_r_re <= _GEN_1
    wire2_r_im <= _GEN_0
    wire2_r_1_re <= _GEN_3
    wire2_r_1_im <= _GEN_2
    wire2_r_2_re <= _GEN_5
    wire2_r_2_im <= _GEN_4
    wire2_r_3_re <= _GEN_7
    wire2_r_3_im <= _GEN_6
    wire2_r_4_re <= _GEN_9
    wire2_r_4_im <= _GEN_8
    wire2_r_5_re <= _GEN_11
    wire2_r_5_im <= _GEN_10
    wire2_r_6_re <= _GEN_13
    wire2_r_6_im <= _GEN_12
    wire2_r_7_re <= _GEN_15
    wire2_r_7_im <= _GEN_14

  module CombFFTwoRev :
    input clock : Clock
    input reset : UInt<1>
    input io_dataIn_re : SInt<32>
    input io_dataIn_im : SInt<32>
    input io_inValid : UInt<1>
    output io_dataOut_re : SInt<32>
    output io_dataOut_im : SInt<32>
    output io_outValid : UInt<1>
    output io_busy : UInt<1>

    inst inst of Commutator @[SDC_SDF_combFFT.scala 90:22]
    inst inst_1 of Commutator_1 @[SDC_SDF_combFFT.scala 90:22]
    inst inst_2 of RealAddSub @[SDC_SDF_combFFT.scala 138:22]
    inst inst_3 of RealBranchMul @[SDC_SDF_combFFT.scala 189:22]
    inst inst_4 of Switch @[SDC_SDF_combFFT.scala 166:22]
    inst inst_5 of Commutator @[SDC_SDF_combFFT.scala 90:22]
    inst stageIntf2_1_inst of RealAdder @[SDC_SDF_combFFT.scala 214:22]
    inst inst_6 of Commutator @[SDC_SDF_combFFT.scala 90:22]
    inst sdfReturn_inst of SDFUnit @[SDC_SDF_combFFT.scala 116:22]
    reg dCount : UInt<6>, clock with :
      reset => (UInt<1>("h0"), dCount) @[SDC_SDF_combFFT.scala 239:23]
    node busy = neq(dCount, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 240:21]
    node _T = or(io_inValid, busy) @[SDC_SDF_combFFT.scala 241:19]
    node _dCount_T = eq(dCount, UInt<6>("h33")) @[SDC_SDF_combFFT.scala 242:26]
    node _dCount_T_1 = add(dCount, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 242:67]
    node _dCount_T_2 = tail(_dCount_T_1, 1) @[SDC_SDF_combFFT.scala 242:67]
    node _dCount_T_3 = mux(_dCount_T, UInt<1>("h0"), _dCount_T_2) @[SDC_SDF_combFFT.scala 242:18]
    node _GEN_0 = mux(_T, _dCount_T_3, dCount) @[SDC_SDF_combFFT.scala 241:27 SDC_SDF_combFFT.scala 242:12 SDC_SDF_combFFT.scala 239:23]
    node _T_1 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 249:71]
    node _T_2 = bits(_T_1, 0, 0) @[SDC_SDF_combFFT.scala 249:81]
    node _T_3 = sub(dCount, UInt<4>("h9")) @[SDC_SDF_combFFT.scala 267:39]
    node _T_4 = tail(_T_3, 1) @[SDC_SDF_combFFT.scala 267:39]
    node _T_5 = bits(_T_4, 3, 0)
    node _T_6 = geq(dCount, UInt<4>("h9")) @[SDC_SDF_combFFT.scala 272:18]
    node _T_7 = lt(dCount, UInt<5>("h19")) @[SDC_SDF_combFFT.scala 272:35]
    node _T_8 = and(_T_6, _T_7) @[SDC_SDF_combFFT.scala 272:25]
    node wIndex_0 = pad(UInt<1>("h0"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_1 = validif(eq(UInt<1>("h0"), _T_5), wIndex_0) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_1 = pad(UInt<1>("h0"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_2 = mux(eq(UInt<1>("h1"), _T_5), wIndex_1, _GEN_1) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_2 = pad(UInt<2>("h2"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_3 = mux(eq(UInt<2>("h2"), _T_5), wIndex_2, _GEN_2) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_3 = pad(UInt<2>("h2"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_4 = mux(eq(UInt<2>("h3"), _T_5), wIndex_3, _GEN_3) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_4 = UInt<3>("h4") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_5 = mux(eq(UInt<3>("h4"), _T_5), wIndex_4, _GEN_4) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_5 = UInt<3>("h4") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_6 = mux(eq(UInt<3>("h5"), _T_5), wIndex_5, _GEN_5) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_6 = UInt<3>("h6") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_7 = mux(eq(UInt<3>("h6"), _T_5), wIndex_6, _GEN_6) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_7 = UInt<3>("h6") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_8 = mux(eq(UInt<3>("h7"), _T_5), wIndex_7, _GEN_7) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_8 = pad(UInt<1>("h1"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_9 = mux(eq(UInt<4>("h8"), _T_5), wIndex_8, _GEN_8) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_9 = pad(UInt<1>("h1"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_10 = mux(eq(UInt<4>("h9"), _T_5), wIndex_9, _GEN_9) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_10 = pad(UInt<2>("h3"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_11 = mux(eq(UInt<4>("ha"), _T_5), wIndex_10, _GEN_10) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_11 = pad(UInt<2>("h3"), 3) @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_12 = mux(eq(UInt<4>("hb"), _T_5), wIndex_11, _GEN_11) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_12 = UInt<3>("h5") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_13 = mux(eq(UInt<4>("hc"), _T_5), wIndex_12, _GEN_12) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_13 = UInt<3>("h5") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_14 = mux(eq(UInt<4>("hd"), _T_5), wIndex_13, _GEN_13) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_14 = UInt<3>("h7") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_15 = mux(eq(UInt<4>("he"), _T_5), wIndex_14, _GEN_14) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node wIndex_15 = UInt<3>("h7") @[SDC_SDF_combFFT.scala 263:25 SDC_SDF_combFFT.scala 263:25]
    node _GEN_16 = mux(eq(UInt<4>("hf"), _T_5), wIndex_15, _GEN_15) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node _wIndex_T_5 = _GEN_16 @[SDC_SDF_combFFT.scala 273:15]
    node re_s_0 = asSInt(UInt<32>("h10000")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_17 = validif(eq(UInt<1>("h0"), _wIndex_T_5), re_s_0) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_1 = asSInt(UInt<32>("hec83")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_18 = mux(eq(UInt<1>("h1"), _wIndex_T_5), re_s_1, _GEN_17) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_2 = asSInt(UInt<32>("hb505")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_19 = mux(eq(UInt<2>("h2"), _wIndex_T_5), re_s_2, _GEN_18) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_3 = asSInt(UInt<32>("h61f8")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_20 = mux(eq(UInt<2>("h3"), _wIndex_T_5), re_s_3, _GEN_19) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_4 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_21 = mux(eq(UInt<3>("h4"), _wIndex_T_5), re_s_4, _GEN_20) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_5 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_22 = mux(eq(UInt<3>("h5"), _wIndex_T_5), re_s_5, _GEN_21) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_6 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_23 = mux(eq(UInt<3>("h6"), _wIndex_T_5), re_s_6, _GEN_22) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node re_s_7 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 233:23 SDC_SDF_combFFT.scala 233:23]
    node _GEN_24 = mux(eq(UInt<3>("h7"), _wIndex_T_5), re_s_7, _GEN_23) @[SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 273:15]
    node _wIndex_T_5_0 = _GEN_16 @[SDC_SDF_combFFT.scala 274:15]
    node im_s_0 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_25 = validif(eq(UInt<1>("h0"), _wIndex_T_5_0), im_s_0) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_1 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_26 = mux(eq(UInt<1>("h1"), _wIndex_T_5_0), im_s_1, _GEN_25) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_2 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_27 = mux(eq(UInt<2>("h2"), _wIndex_T_5_0), im_s_2, _GEN_26) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_3 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_28 = mux(eq(UInt<2>("h3"), _wIndex_T_5_0), im_s_3, _GEN_27) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_4 = asSInt(UInt<32>("hffff0000")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_29 = mux(eq(UInt<3>("h4"), _wIndex_T_5_0), im_s_4, _GEN_28) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_5 = asSInt(UInt<32>("hffff137d")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_30 = mux(eq(UInt<3>("h5"), _wIndex_T_5_0), im_s_5, _GEN_29) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_6 = asSInt(UInt<32>("hffff4afb")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_31 = mux(eq(UInt<3>("h6"), _wIndex_T_5_0), im_s_6, _GEN_30) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node im_s_7 = asSInt(UInt<32>("hffff9e08")) @[SDC_SDF_combFFT.scala 234:23 SDC_SDF_combFFT.scala 234:23]
    node _GEN_32 = mux(eq(UInt<3>("h7"), _wIndex_T_5_0), im_s_7, _GEN_31) @[SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 274:15]
    node _re_s_wIndex_T_5 = _GEN_24 @[SDC_SDF_combFFT.scala 273:15]
    node _GEN_33 = mux(_T_8, _re_s_wIndex_T_5, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 272:56 SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 276:15]
    node _im_s_wIndex_T_5_0 = _GEN_32 @[SDC_SDF_combFFT.scala 274:15]
    node _GEN_34 = mux(_T_8, _im_s_wIndex_T_5_0, asSInt(UInt<32>("h0"))) @[SDC_SDF_combFFT.scala 272:56 SDC_SDF_combFFT.scala 274:15 SDC_SDF_combFFT.scala 277:15]
    node _T_9 = geq(dCount, UInt<4>("h9")) @[SDC_SDF_combFFT.scala 283:18]
    node _wireSelectDC_T = sub(dCount, UInt<4>("h9")) @[SDC_SDF_combFFT.scala 284:31]
    node _wireSelectDC_T_1 = tail(_wireSelectDC_T, 1) @[SDC_SDF_combFFT.scala 284:31]
    node _wireSelectDC_T_2 = bits(_wireSelectDC_T_1, 3, 3) @[SDC_SDF_combFFT.scala 284:36]
    node _wireSelectDC_T_3 = bits(_wireSelectDC_T_2, 0, 0) @[SDC_SDF_combFFT.scala 284:60]
    node _wireSelectDC_T_4 = eq(_wireSelectDC_T_3, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 284:23]
    node _GEN_35 = mux(_T_9, _wireSelectDC_T_4, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 283:26 SDC_SDF_combFFT.scala 284:20 SDC_SDF_combFFT.scala 286:20]
    node _commonSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 295:50]
    node _commonSelt_T_1 = bits(_commonSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 295:60]
    node _commonSelt_T_2 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 295:71]
    node _commonSelt_T_3 = bits(_commonSelt_T_2, 0, 0) @[SDC_SDF_combFFT.scala 295:81]
    node _commonSelt_T_4 = eq(_commonSelt_T_3, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 295:64]
    node _commonSelt_T_5 = mux(UInt<1>("h1"), _commonSelt_T_1, _commonSelt_T_4) @[SDC_SDF_combFFT.scala 295:22]
    reg stageIntf1_1_REG : SInt<32>, clock with :
      reset => (UInt<1>("h0"), stageIntf1_1_REG) @[SDC_SDF_combFFT.scala 302:33]
    node _postStgSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 305:58]
    node _postStgSelt_T_1 = bits(_postStgSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 305:68]
    node _postStgSelt_T_2 = eq(_postStgSelt_T_1, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 305:51]
    node _postStgSelt_T_3 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 305:78]
    node _postStgSelt_T_4 = bits(_postStgSelt_T_3, 0, 0) @[SDC_SDF_combFFT.scala 305:88]
    node postStgSelt = mux(UInt<1>("h0"), _postStgSelt_T_2, _postStgSelt_T_4) @[SDC_SDF_combFFT.scala 305:24]
    node _T_10 = geq(dCount, UInt<5>("h13")) @[SDC_SDF_combFFT.scala 313:16]
    node _GEN_36 = mux(_T_10, UInt<1>("h1"), UInt<1>("h0")) @[SDC_SDF_combFFT.scala 313:47 SDC_SDF_combFFT.scala 314:15 SDC_SDF_combFFT.scala 316:15]
    reg io_outValid_REG : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_outValid_REG) @[SDC_SDF_combFFT.scala 323:25]
    node _io_outValid_T = eq(io_outValid_REG, UInt<6>("h22")) @[SDC_SDF_combFFT.scala 323:34]
    node stageIntf1_0 = inst.io_out1 @[SDC_SDF_combFFT.scala 246:27 SDC_SDF_combFFT.scala 250:17]
    node stageIntf1_1 = stageIntf1_1_REG @[SDC_SDF_combFFT.scala 246:27 SDC_SDF_combFFT.scala 302:23]
    node stageIntf1_2 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 246:27 SDC_SDF_combFFT.scala 246:27]
    node stageIntf1_3 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 246:27 SDC_SDF_combFFT.scala 246:27]
    node stageIntf2_0 = inst.io_out2 @[SDC_SDF_combFFT.scala 247:27 SDC_SDF_combFFT.scala 251:17]
    node stageIntf2_1 = stageIntf2_1_inst.io_out @[SDC_SDF_combFFT.scala 247:27 SDC_SDF_combFFT.scala 301:23]
    node stageIntf2_2 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 247:27 SDC_SDF_combFFT.scala 247:27]
    node stageIntf2_3 = asSInt(UInt<32>("h0")) @[SDC_SDF_combFFT.scala 247:27 SDC_SDF_combFFT.scala 247:27]
    node wireWRe = _GEN_33 @[SDC_SDF_combFFT.scala 270:23]
    node wireWIm = _GEN_34 @[SDC_SDF_combFFT.scala 271:23]
    node wireSelectDC = _GEN_35 @[SDC_SDF_combFFT.scala 282:28]
    node commonSelt = _commonSelt_T_5 @[SDC_SDF_combFFT.scala 294:26 SDC_SDF_combFFT.scala 295:16]
    node wireFpToComp_re = inst_6.io_out1 @[SDC_SDF_combFFT.scala 308:26 SDC_SDF_combFFT.scala 309:19]
    node wireFpToComp_im = inst_6.io_out2 @[SDC_SDF_combFFT.scala 308:26 SDC_SDF_combFFT.scala 310:19]
    node sdfSelect = _GEN_36 @[SDC_SDF_combFFT.scala 312:23]
    io_dataOut_re <= inst_1.io_out1 @[SDC_SDF_combFFT.scala 321:17]
    io_dataOut_im <= inst_1.io_out2 @[SDC_SDF_combFFT.scala 322:17]
    io_outValid <= _io_outValid_T @[SDC_SDF_combFFT.scala 323:15]
    io_busy <= busy @[SDC_SDF_combFFT.scala 244:11]
    dCount <= mux(reset, UInt<6>("h0"), _GEN_0) @[SDC_SDF_combFFT.scala 239:23 SDC_SDF_combFFT.scala 239:23]
    inst.clock <= clock
    inst.reset <= reset
    inst.io_in1 <= io_dataIn_re @[SDC_SDF_combFFT.scala 91:17]
    inst.io_in2 <= io_dataIn_im @[SDC_SDF_combFFT.scala 92:17]
    inst.io_s <= _T_2 @[SDC_SDF_combFFT.scala 93:15]
    inst_1.clock <= clock
    inst_1.reset <= reset
    inst_1.io_in1 <= stageIntf1_0 @[SDC_SDF_combFFT.scala 91:17]
    inst_1.io_in2 <= stageIntf2_0 @[SDC_SDF_combFFT.scala 92:17]
    inst_1.io_s <= wireSelectDC @[SDC_SDF_combFFT.scala 93:15]
    inst_2.clock <= clock
    inst_2.reset <= reset
    inst_2.io_a <= inst_1.io_out1 @[SDC_SDF_combFFT.scala 139:15]
    inst_2.io_b <= inst_1.io_out2 @[SDC_SDF_combFFT.scala 140:15]
    inst_3.clock <= clock
    inst_3.reset <= reset
    inst_3.io_dataIn <= inst_2.io_B @[SDC_SDF_combFFT.scala 190:20]
    inst_3.io_w_re_in <= wireWRe @[SDC_SDF_combFFT.scala 191:21]
    inst_3.io_w_im_in <= wireWIm @[SDC_SDF_combFFT.scala 192:21]
    inst_4.clock <= clock
    inst_4.reset <= reset
    inst_4.io_s <= commonSelt @[SDC_SDF_combFFT.scala 169:15]
    inst_4.io_in1 <= inst_3.io_out1 @[SDC_SDF_combFFT.scala 167:17]
    inst_4.io_in2 <= inst_3.io_out2 @[SDC_SDF_combFFT.scala 168:17]
    inst_5.clock <= clock
    inst_5.reset <= reset
    inst_5.io_in1 <= inst_4.io_out1 @[SDC_SDF_combFFT.scala 91:17]
    inst_5.io_in2 <= inst_4.io_out2 @[SDC_SDF_combFFT.scala 92:17]
    inst_5.io_s <= commonSelt @[SDC_SDF_combFFT.scala 93:15]
    stageIntf2_1_inst.clock <= clock
    stageIntf2_1_inst.reset <= reset
    stageIntf2_1_inst.io_in1 <= inst_5.io_out1 @[SDC_SDF_combFFT.scala 215:17]
    stageIntf2_1_inst.io_in2 <= inst_5.io_out2 @[SDC_SDF_combFFT.scala 216:17]
    stageIntf2_1_inst.io_s <= commonSelt @[SDC_SDF_combFFT.scala 217:15]
    stageIntf1_1_REG <= inst_2.io_A @[SDC_SDF_combFFT.scala 302:33]
    inst_6.clock <= clock
    inst_6.reset <= reset
    inst_6.io_in1 <= stageIntf1_3 @[SDC_SDF_combFFT.scala 91:17]
    inst_6.io_in2 <= stageIntf2_3 @[SDC_SDF_combFFT.scala 92:17]
    inst_6.io_s <= postStgSelt @[SDC_SDF_combFFT.scala 93:15]
    sdfReturn_inst.clock <= clock
    sdfReturn_inst.reset <= reset
    sdfReturn_inst.io_in_re <= wireFpToComp_re @[SDC_SDF_combFFT.scala 117:16]
    sdfReturn_inst.io_in_im <= wireFpToComp_im @[SDC_SDF_combFFT.scala 117:16]
    sdfReturn_inst.io_ctrl <= sdfSelect @[SDC_SDF_combFFT.scala 118:18]
    io_outValid_REG <= dCount @[SDC_SDF_combFFT.scala 323:25]
