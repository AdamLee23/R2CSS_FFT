circuit SDFUnit :
  module ComplexAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = add(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 39:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 39:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 39:26]
    node _io_res_im_T = add(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 40:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 39:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 40:13]

  module ComplexSub :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>

    node _io_res_re_T = sub(io_op1_re, io_op2_re) @[SDC_SDF_combFFT.scala 56:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 56:26]
    node _io_res_re_T_2 = asSInt(_io_res_re_T_1) @[SDC_SDF_combFFT.scala 56:26]
    node _io_res_im_T = sub(io_op1_im, io_op2_im) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_im_T_2 = asSInt(_io_res_im_T_1) @[SDC_SDF_combFFT.scala 57:26]
    io_res_re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 56:13]
    io_res_im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 57:13]

  module SDFUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>

    inst wire1_inst of ComplexAdd @[SDC_SDF_combFFT.scala 46:22]
    inst io_out_inst of ComplexSub @[SDC_SDF_combFFT.scala 63:22]
    reg wire2_r_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_re) @[Reg.scala 15:16]
    reg wire2_r_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_im) @[Reg.scala 15:16]
    node wire1_im = wire1_inst.io_res_im @[SDC_SDF_combFFT.scala 91:19 SDC_SDF_combFFT.scala 93:9]
    node _GEN_0 = mux(UInt<1>("h1"), wire1_im, wire2_r_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire1_re = wire1_inst.io_res_re @[SDC_SDF_combFFT.scala 91:19 SDC_SDF_combFFT.scala 93:9]
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
    reg wire2_r_7_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_re) @[Reg.scala 15:16]
    reg wire2_r_7_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), wire2_r_7_im) @[Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), wire2_r_6_im, wire2_r_7_im) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node _GEN_15 = mux(UInt<1>("h1"), wire2_r_6_re, wire2_r_7_re) @[Reg.scala 16:19 Reg.scala 16:23 Reg.scala 15:16]
    node wire2_re = wire2_r_7_re @[SDC_SDF_combFFT.scala 92:19 SDC_SDF_combFFT.scala 95:9]
    node wire2_im = wire2_r_7_im @[SDC_SDF_combFFT.scala 92:19 SDC_SDF_combFFT.scala 95:9]
    io_out_re <= io_out_inst.io_res_re @[SDC_SDF_combFFT.scala 94:10]
    io_out_im <= io_out_inst.io_res_im @[SDC_SDF_combFFT.scala 94:10]
    wire1_inst.clock <= clock
    wire1_inst.reset <= reset
    wire1_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 47:17]
    wire1_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 47:17]
    wire1_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 48:17]
    io_out_inst.clock <= clock
    io_out_inst.reset <= reset
    io_out_inst.io_op1_re <= wire2_re @[SDC_SDF_combFFT.scala 64:17]
    io_out_inst.io_op1_im <= wire2_im @[SDC_SDF_combFFT.scala 64:17]
    io_out_inst.io_op2_re <= io_in_re @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io_op2_im <= io_in_im @[SDC_SDF_combFFT.scala 65:17]
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
