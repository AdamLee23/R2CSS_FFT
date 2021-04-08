;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit SDFUnit : 
  module ComplexAdd : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _io_res_re_T = add(io.op1.re, io.op2.re) @[SDC_SDF_combFFT.scala 39:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 39:26]
    node _io_res_re_T_2 = asFixedPoint(_io_res_re_T_1, 16) @[SDC_SDF_combFFT.scala 39:26]
    io.res.re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 39:13]
    node _io_res_im_T = add(io.op1.im, io.op2.im) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_im_T_2 = asFixedPoint(_io_res_im_T_1, 16) @[SDC_SDF_combFFT.scala 40:26]
    io.res.im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 40:13]
    
  module ComplexSub : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _io_res_re_T = sub(io.op1.re, io.op2.re) @[SDC_SDF_combFFT.scala 56:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 56:26]
    node _io_res_re_T_2 = asFixedPoint(_io_res_re_T_1, 16) @[SDC_SDF_combFFT.scala 56:26]
    io.res.re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 56:13]
    node _io_res_im_T = sub(io.op1.im, io.op2.im) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_im_T_2 = asFixedPoint(_io_res_im_T_1, 16) @[SDC_SDF_combFFT.scala 57:26]
    io.res.im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 57:13]
    
  module SDFUnit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    wire wire1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[SDC_SDF_combFFT.scala 91:19]
    wire wire2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[SDC_SDF_combFFT.scala 92:19]
    inst wire1_inst of ComplexAdd @[SDC_SDF_combFFT.scala 46:22]
    wire1_inst.clock <= clock
    wire1_inst.reset <= reset
    wire1_inst.io.op1.im <= wire2.im @[SDC_SDF_combFFT.scala 47:17]
    wire1_inst.io.op1.re <= wire2.re @[SDC_SDF_combFFT.scala 47:17]
    wire1_inst.io.op2.im <= io.in.im @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io.op2.re <= io.in.re @[SDC_SDF_combFFT.scala 48:17]
    wire1.im <= wire1_inst.io.res.im @[SDC_SDF_combFFT.scala 93:9]
    wire1.re <= wire1_inst.io.res.re @[SDC_SDF_combFFT.scala 93:9]
    inst io_out_inst of ComplexSub @[SDC_SDF_combFFT.scala 63:22]
    io_out_inst.clock <= clock
    io_out_inst.reset <= reset
    io_out_inst.io.op1.im <= wire2.im @[SDC_SDF_combFFT.scala 64:17]
    io_out_inst.io.op1.re <= wire2.re @[SDC_SDF_combFFT.scala 64:17]
    io_out_inst.io.op2.im <= io.in.im @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io.op2.re <= io.in.re @[SDC_SDF_combFFT.scala 65:17]
    io.out.im <= io_out_inst.io.res.im @[SDC_SDF_combFFT.scala 94:10]
    io.out.re <= io_out_inst.io.res.re @[SDC_SDF_combFFT.scala 94:10]
    reg wire2_r : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r.im <= wire1.im @[Reg.scala 16:23]
      wire2_r.re <= wire1.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_1.im <= wire2_r.im @[Reg.scala 16:23]
      wire2_r_1.re <= wire2_r.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_2.im <= wire2_r_1.im @[Reg.scala 16:23]
      wire2_r_2.re <= wire2_r_1.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_3 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_3.im <= wire2_r_2.im @[Reg.scala 16:23]
      wire2_r_3.re <= wire2_r_2.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_4 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_4.im <= wire2_r_3.im @[Reg.scala 16:23]
      wire2_r_4.re <= wire2_r_3.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_5 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_5.im <= wire2_r_4.im @[Reg.scala 16:23]
      wire2_r_5.re <= wire2_r_4.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_6 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_6.im <= wire2_r_5.im @[Reg.scala 16:23]
      wire2_r_6.re <= wire2_r_5.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire2_r_7 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire2_r_7.im <= wire2_r_6.im @[Reg.scala 16:23]
      wire2_r_7.re <= wire2_r_6.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire2.im <= wire2_r_7.im @[SDC_SDF_combFFT.scala 95:9]
    wire2.re <= wire2_r_7.re @[SDC_SDF_combFFT.scala 95:9]
    
