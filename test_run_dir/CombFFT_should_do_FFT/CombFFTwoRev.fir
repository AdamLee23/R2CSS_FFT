;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit CombFFTwoRev : 
  module Commutator : 
    input clock : Clock
    input reset : Reset
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    wire wire1 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 79:19]
    wire wire2 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 80:19]
    reg wire1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r <= io.in2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire1 <= wire1_r @[SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io.s, wire1, io.in1) @[SDC_SDF_combFFT.scala 82:15]
    wire2 <= _wire2_T @[SDC_SDF_combFFT.scala 82:9]
    reg io_out1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r <= wire2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out1 <= io_out1_r @[SDC_SDF_combFFT.scala 83:11]
    node _io_out2_T = mux(io.s, io.in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
    
  module Commutator_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    wire wire1 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 79:19]
    wire wire2 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 80:19]
    reg wire1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r <= io.in2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_1 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_1 <= wire1_r @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_2 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_2 <= wire1_r_1 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_3 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_3 <= wire1_r_2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_4 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_4 <= wire1_r_3 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_5 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_5 <= wire1_r_4 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_6 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_6 <= wire1_r_5 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_7 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_7 <= wire1_r_6 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire1 <= wire1_r_7 @[SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io.s, wire1, io.in1) @[SDC_SDF_combFFT.scala 82:15]
    wire2 <= _wire2_T @[SDC_SDF_combFFT.scala 82:9]
    reg io_out1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r <= wire2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_1 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_1 <= io_out1_r @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_2 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_2 <= io_out1_r_1 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_3 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_3 <= io_out1_r_2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_4 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_4 <= io_out1_r_3 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_5 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_5 <= io_out1_r_4 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_6 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_6 <= io_out1_r_5 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_7 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_7 <= io_out1_r_6 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out1 <= io_out1_r_7 @[SDC_SDF_combFFT.scala 83:11]
    node _io_out2_T = mux(io.s, io.in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
    
  module RealAddSub : 
    input clock : Clock
    input reset : Reset
    output io : {flip a : Fixed<32><<16>>, flip b : Fixed<32><<16>>, A : Fixed<32><<16>>, B : Fixed<32><<16>>}
    
    node _io_A_T = add(io.a, io.b) @[SDC_SDF_combFFT.scala 131:16]
    node _io_A_T_1 = tail(_io_A_T, 1) @[SDC_SDF_combFFT.scala 131:16]
    node _io_A_T_2 = asFixedPoint(_io_A_T_1, 16) @[SDC_SDF_combFFT.scala 131:16]
    io.A <= _io_A_T_2 @[SDC_SDF_combFFT.scala 131:8]
    node _io_B_T = sub(io.a, io.b) @[SDC_SDF_combFFT.scala 132:16]
    node _io_B_T_1 = tail(_io_B_T, 1) @[SDC_SDF_combFFT.scala 132:16]
    node _io_B_T_2 = asFixedPoint(_io_B_T_1, 16) @[SDC_SDF_combFFT.scala 132:16]
    io.B <= _io_B_T_2 @[SDC_SDF_combFFT.scala 132:8]
    
  module RealBranchMul : 
    input clock : Clock
    input reset : Reset
    output io : {flip dataIn : Fixed<32><<16>>, flip w_re_in : Fixed<32><<16>>, flip w_im_in : Fixed<32><<16>>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    node _io_out1_T = mul(io.w_re_in, io.dataIn) @[SDC_SDF_combFFT.scala 182:25]
    io.out1 <= _io_out1_T @[SDC_SDF_combFFT.scala 182:11]
    node _io_out2_T = mul(io.w_im_in, io.dataIn) @[SDC_SDF_combFFT.scala 183:25]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 183:11]
    
  module Switch : 
    input clock : Clock
    input reset : Reset
    output io : {flip s : UInt<1>, flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    when io.s : @[SDC_SDF_combFFT.scala 154:15]
      io.out1 <= io.in1 @[SDC_SDF_combFFT.scala 155:13]
      io.out2 <= io.in2 @[SDC_SDF_combFFT.scala 156:13]
      skip @[SDC_SDF_combFFT.scala 154:15]
    else : @[SDC_SDF_combFFT.scala 157:5]
      io.out1 <= io.in2 @[SDC_SDF_combFFT.scala 158:13]
      io.out2 <= io.in1 @[SDC_SDF_combFFT.scala 159:13]
      skip @[SDC_SDF_combFFT.scala 157:5]
    
  module Commutator_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    wire wire1 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 79:19]
    wire wire2 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 80:19]
    reg wire1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r <= io.in2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire1 <= wire1_r @[SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io.s, wire1, io.in1) @[SDC_SDF_combFFT.scala 82:15]
    wire2 <= _wire2_T @[SDC_SDF_combFFT.scala 82:9]
    reg io_out1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r <= wire2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out1 <= io_out1_r @[SDC_SDF_combFFT.scala 83:11]
    node _io_out2_T = mux(io.s, io.in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
    
  module RealAdder : 
    input clock : Clock
    input reset : Reset
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out : Fixed<32><<16>>}
    
    when io.s : @[SDC_SDF_combFFT.scala 204:15]
      node _io_out_T = sub(io.in1, io.in2) @[SDC_SDF_combFFT.scala 205:22]
      node _io_out_T_1 = tail(_io_out_T, 1) @[SDC_SDF_combFFT.scala 205:22]
      node _io_out_T_2 = asFixedPoint(_io_out_T_1, 16) @[SDC_SDF_combFFT.scala 205:22]
      io.out <= _io_out_T_2 @[SDC_SDF_combFFT.scala 205:12]
      skip @[SDC_SDF_combFFT.scala 204:15]
    else : @[SDC_SDF_combFFT.scala 206:5]
      node _io_out_T_3 = add(io.in1, io.in2) @[SDC_SDF_combFFT.scala 207:22]
      node _io_out_T_4 = tail(_io_out_T_3, 1) @[SDC_SDF_combFFT.scala 207:22]
      node _io_out_T_5 = asFixedPoint(_io_out_T_4, 16) @[SDC_SDF_combFFT.scala 207:22]
      io.out <= _io_out_T_5 @[SDC_SDF_combFFT.scala 207:12]
      skip @[SDC_SDF_combFFT.scala 206:5]
    
  module Commutator_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    wire wire1 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 79:19]
    wire wire2 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 80:19]
    reg wire1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r <= io.in2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire1 <= wire1_r @[SDC_SDF_combFFT.scala 81:9]
    node _wire2_T = mux(io.s, wire1, io.in1) @[SDC_SDF_combFFT.scala 82:15]
    wire2 <= _wire2_T @[SDC_SDF_combFFT.scala 82:9]
    reg io_out1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r <= wire2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out1 <= io_out1_r @[SDC_SDF_combFFT.scala 83:11]
    node _io_out2_T = mux(io.s, io.in1, wire1) @[SDC_SDF_combFFT.scala 84:17]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 84:11]
    
  module ComplexAdd : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _io_res_re_T = add(io.op1.re, io.op2.re) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 40:26]
    node _io_res_re_T_2 = asFixedPoint(_io_res_re_T_1, 16) @[SDC_SDF_combFFT.scala 40:26]
    io.res.re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 40:13]
    node _io_res_im_T = add(io.op1.im, io.op2.im) @[SDC_SDF_combFFT.scala 41:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 41:26]
    node _io_res_im_T_2 = asFixedPoint(_io_res_im_T_1, 16) @[SDC_SDF_combFFT.scala 41:26]
    io.res.im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 41:13]
    
  module ComplexSub : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _io_res_re_T = sub(io.op1.re, io.op2.re) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_re_T_1 = tail(_io_res_re_T, 1) @[SDC_SDF_combFFT.scala 57:26]
    node _io_res_re_T_2 = asFixedPoint(_io_res_re_T_1, 16) @[SDC_SDF_combFFT.scala 57:26]
    io.res.re <= _io_res_re_T_2 @[SDC_SDF_combFFT.scala 57:13]
    node _io_res_im_T = sub(io.op1.im, io.op2.im) @[SDC_SDF_combFFT.scala 58:26]
    node _io_res_im_T_1 = tail(_io_res_im_T, 1) @[SDC_SDF_combFFT.scala 58:26]
    node _io_res_im_T_2 = asFixedPoint(_io_res_im_T_1, 16) @[SDC_SDF_combFFT.scala 58:26]
    io.res.im <= _io_res_im_T_2 @[SDC_SDF_combFFT.scala 58:13]
    
  module SDFUnit : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip ctrl : UInt<1>, out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    wire wire1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[SDC_SDF_combFFT.scala 104:19]
    wire wire2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[SDC_SDF_combFFT.scala 105:19]
    inst wire1_inst of ComplexAdd @[SDC_SDF_combFFT.scala 47:22]
    wire1_inst.clock <= clock
    wire1_inst.reset <= reset
    wire1_inst.io.op1.im <= wire2.im @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io.op1.re <= wire2.re @[SDC_SDF_combFFT.scala 48:17]
    wire1_inst.io.op2.im <= io.in.im @[SDC_SDF_combFFT.scala 49:17]
    wire1_inst.io.op2.re <= io.in.re @[SDC_SDF_combFFT.scala 49:17]
    wire1.im <= wire1_inst.io.res.im @[SDC_SDF_combFFT.scala 106:9]
    wire1.re <= wire1_inst.io.res.re @[SDC_SDF_combFFT.scala 106:9]
    inst io_out_inst of ComplexSub @[SDC_SDF_combFFT.scala 64:22]
    io_out_inst.clock <= clock
    io_out_inst.reset <= reset
    io_out_inst.io.op1.im <= wire2.im @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io.op1.re <= wire2.re @[SDC_SDF_combFFT.scala 65:17]
    io_out_inst.io.op2.im <= io.in.im @[SDC_SDF_combFFT.scala 66:17]
    io_out_inst.io.op2.re <= io.in.re @[SDC_SDF_combFFT.scala 66:17]
    node _io_out_T = mux(io.ctrl, wire2, io_out_inst.io.res) @[SDC_SDF_combFFT.scala 109:16]
    io.out.im <= _io_out_T.im @[SDC_SDF_combFFT.scala 109:10]
    io.out.re <= _io_out_T.re @[SDC_SDF_combFFT.scala 109:10]
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
    wire2.im <= wire2_r_7.im @[SDC_SDF_combFFT.scala 110:9]
    wire2.re <= wire2_r_7.re @[SDC_SDF_combFFT.scala 110:9]
    
  module CombFFTwoRev : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip dataIn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip inValid : UInt<1>, dataOut : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, outValid : UInt<1>, busy : UInt<1>}
    
    reg dCount : UInt<6>, clock with : (reset => (reset, UInt<6>("h00"))) @[SDC_SDF_combFFT.scala 239:23]
    node busy = neq(dCount, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 240:21]
    node _T = or(io.inValid, busy) @[SDC_SDF_combFFT.scala 241:19]
    when _T : @[SDC_SDF_combFFT.scala 241:27]
      node _dCount_T = eq(dCount, UInt<6>("h033")) @[SDC_SDF_combFFT.scala 242:26]
      node _dCount_T_1 = add(dCount, UInt<1>("h01")) @[SDC_SDF_combFFT.scala 242:67]
      node _dCount_T_2 = tail(_dCount_T_1, 1) @[SDC_SDF_combFFT.scala 242:67]
      node _dCount_T_3 = mux(_dCount_T, UInt<1>("h00"), _dCount_T_2) @[SDC_SDF_combFFT.scala 242:18]
      dCount <= _dCount_T_3 @[SDC_SDF_combFFT.scala 242:12]
      skip @[SDC_SDF_combFFT.scala 241:27]
    io.busy <= busy @[SDC_SDF_combFFT.scala 244:11]
    wire stageIntf1 : Fixed<32><<16>>[4] @[SDC_SDF_combFFT.scala 246:27]
    stageIntf1[0] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 246:27]
    stageIntf1[1] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 246:27]
    stageIntf1[2] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 246:27]
    stageIntf1[3] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 246:27]
    wire stageIntf2 : Fixed<32><<16>>[4] @[SDC_SDF_combFFT.scala 247:27]
    stageIntf2[0] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 247:27]
    stageIntf2[1] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 247:27]
    stageIntf2[2] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 247:27]
    stageIntf2[3] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 247:27]
    node _T_1 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 249:71]
    node _T_2 = bits(_T_1, 0, 0) @[SDC_SDF_combFFT.scala 249:81]
    inst inst of Commutator @[SDC_SDF_combFFT.scala 90:22]
    inst.clock <= clock
    inst.reset <= reset
    inst.io.in1 <= io.dataIn.re @[SDC_SDF_combFFT.scala 91:17]
    inst.io.in2 <= io.dataIn.im @[SDC_SDF_combFFT.scala 92:17]
    inst.io.s <= _T_2 @[SDC_SDF_combFFT.scala 93:15]
    stageIntf1[0] <= inst.io.out1 @[SDC_SDF_combFFT.scala 250:17]
    stageIntf2[0] <= inst.io.out2 @[SDC_SDF_combFFT.scala 251:17]
    wire wIndex : UInt<3>[16] @[SDC_SDF_combFFT.scala 263:25]
    wIndex[0] <= UInt<1>("h00") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[1] <= UInt<1>("h00") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[2] <= UInt<2>("h02") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[3] <= UInt<2>("h02") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[4] <= UInt<3>("h04") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[5] <= UInt<3>("h04") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[6] <= UInt<3>("h06") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[7] <= UInt<3>("h06") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[8] <= UInt<1>("h01") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[9] <= UInt<1>("h01") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[10] <= UInt<2>("h03") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[11] <= UInt<2>("h03") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[12] <= UInt<3>("h05") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[13] <= UInt<3>("h05") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[14] <= UInt<3>("h07") @[SDC_SDF_combFFT.scala 263:25]
    wIndex[15] <= UInt<3>("h07") @[SDC_SDF_combFFT.scala 263:25]
    node _T_3 = sub(dCount, UInt<4>("h09")) @[SDC_SDF_combFFT.scala 267:39]
    node _T_4 = tail(_T_3, 1) @[SDC_SDF_combFFT.scala 267:39]
    node _T_5 = bits(_T_4, 3, 0)
    wire re_s : Fixed<32><<16>>[8] @[SDC_SDF_combFFT.scala 233:23]
    re_s[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[1] <= asFixedPoint(UInt<32>("h0ec83"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[2] <= asFixedPoint(UInt<32>("h0b505"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[3] <= asFixedPoint(UInt<32>("h061f8"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[4] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[5] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[SDC_SDF_combFFT.scala 233:23]
    re_s[7] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[SDC_SDF_combFFT.scala 233:23]
    wire im_s : Fixed<32><<16>>[8] @[SDC_SDF_combFFT.scala 234:23]
    im_s[0] <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[1] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[2] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[3] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[4] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[5] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[SDC_SDF_combFFT.scala 234:23]
    im_s[7] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[SDC_SDF_combFFT.scala 234:23]
    wire wireWRe : Fixed @[SDC_SDF_combFFT.scala 270:23]
    wire wireWIm : Fixed @[SDC_SDF_combFFT.scala 271:23]
    node _T_6 = geq(dCount, UInt<4>("h09")) @[SDC_SDF_combFFT.scala 272:18]
    node _T_7 = lt(dCount, UInt<5>("h019")) @[SDC_SDF_combFFT.scala 272:35]
    node _T_8 = and(_T_6, _T_7) @[SDC_SDF_combFFT.scala 272:25]
    when _T_8 : @[SDC_SDF_combFFT.scala 272:56]
      wireWRe <= re_s[wIndex[_T_5]] @[SDC_SDF_combFFT.scala 273:15]
      wireWIm <= im_s[wIndex[_T_5]] @[SDC_SDF_combFFT.scala 274:15]
      skip @[SDC_SDF_combFFT.scala 272:56]
    else : @[SDC_SDF_combFFT.scala 275:7]
      wireWRe <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 276:15]
      wireWIm <= asFixedPoint(UInt<32>("h00"), 16) @[SDC_SDF_combFFT.scala 277:15]
      skip @[SDC_SDF_combFFT.scala 275:7]
    wire wireSelectDC : UInt<1> @[SDC_SDF_combFFT.scala 282:28]
    node _T_9 = geq(dCount, UInt<4>("h09")) @[SDC_SDF_combFFT.scala 283:18]
    when _T_9 : @[SDC_SDF_combFFT.scala 283:26]
      node _wireSelectDC_T = sub(dCount, UInt<4>("h09")) @[SDC_SDF_combFFT.scala 284:31]
      node _wireSelectDC_T_1 = tail(_wireSelectDC_T, 1) @[SDC_SDF_combFFT.scala 284:31]
      node _wireSelectDC_T_2 = bits(_wireSelectDC_T_1, 3, 3) @[SDC_SDF_combFFT.scala 284:36]
      node _wireSelectDC_T_3 = bits(_wireSelectDC_T_2, 0, 0) @[SDC_SDF_combFFT.scala 284:60]
      node _wireSelectDC_T_4 = eq(_wireSelectDC_T_3, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 284:23]
      wireSelectDC <= _wireSelectDC_T_4 @[SDC_SDF_combFFT.scala 284:20]
      skip @[SDC_SDF_combFFT.scala 283:26]
    else : @[SDC_SDF_combFFT.scala 285:7]
      wireSelectDC <= UInt<1>("h00") @[SDC_SDF_combFFT.scala 286:20]
      skip @[SDC_SDF_combFFT.scala 285:7]
    inst inst_1 of Commutator_1 @[SDC_SDF_combFFT.scala 90:22]
    inst_1.clock <= clock
    inst_1.reset <= reset
    inst_1.io.in1 <= stageIntf1[0] @[SDC_SDF_combFFT.scala 91:17]
    inst_1.io.in2 <= stageIntf2[0] @[SDC_SDF_combFFT.scala 92:17]
    inst_1.io.s <= wireSelectDC @[SDC_SDF_combFFT.scala 93:15]
    inst inst_2 of RealAddSub @[SDC_SDF_combFFT.scala 138:22]
    inst_2.clock <= clock
    inst_2.reset <= reset
    inst_2.io.a <= inst_1.io.out1 @[SDC_SDF_combFFT.scala 139:15]
    inst_2.io.b <= inst_1.io.out2 @[SDC_SDF_combFFT.scala 140:15]
    inst inst_3 of RealBranchMul @[SDC_SDF_combFFT.scala 189:22]
    inst_3.clock <= clock
    inst_3.reset <= reset
    inst_3.io.dataIn <= inst_2.io.B @[SDC_SDF_combFFT.scala 190:20]
    inst_3.io.w_re_in <= wireWRe @[SDC_SDF_combFFT.scala 191:21]
    inst_3.io.w_im_in <= wireWIm @[SDC_SDF_combFFT.scala 192:21]
    wire commonSelt : UInt<1> @[SDC_SDF_combFFT.scala 294:26]
    node _commonSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 295:50]
    node _commonSelt_T_1 = bits(_commonSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 295:60]
    node _commonSelt_T_2 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 295:71]
    node _commonSelt_T_3 = bits(_commonSelt_T_2, 0, 0) @[SDC_SDF_combFFT.scala 295:81]
    node _commonSelt_T_4 = eq(_commonSelt_T_3, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 295:64]
    node _commonSelt_T_5 = mux(UInt<1>("h01"), _commonSelt_T_1, _commonSelt_T_4) @[SDC_SDF_combFFT.scala 295:22]
    commonSelt <= _commonSelt_T_5 @[SDC_SDF_combFFT.scala 295:16]
    inst inst_4 of Switch @[SDC_SDF_combFFT.scala 166:22]
    inst_4.clock <= clock
    inst_4.reset <= reset
    inst_4.io.in1 <= inst_3.io.out1 @[SDC_SDF_combFFT.scala 167:17]
    inst_4.io.in2 <= inst_3.io.out2 @[SDC_SDF_combFFT.scala 168:17]
    inst_4.io.s <= commonSelt @[SDC_SDF_combFFT.scala 169:15]
    inst inst_5 of Commutator_2 @[SDC_SDF_combFFT.scala 90:22]
    inst_5.clock <= clock
    inst_5.reset <= reset
    inst_5.io.in1 <= inst_4.io.out1 @[SDC_SDF_combFFT.scala 91:17]
    inst_5.io.in2 <= inst_4.io.out2 @[SDC_SDF_combFFT.scala 92:17]
    inst_5.io.s <= commonSelt @[SDC_SDF_combFFT.scala 93:15]
    inst stageIntf2_1_inst of RealAdder @[SDC_SDF_combFFT.scala 214:22]
    stageIntf2_1_inst.clock <= clock
    stageIntf2_1_inst.reset <= reset
    stageIntf2_1_inst.io.in1 <= inst_5.io.out1 @[SDC_SDF_combFFT.scala 215:17]
    stageIntf2_1_inst.io.in2 <= inst_5.io.out2 @[SDC_SDF_combFFT.scala 216:17]
    stageIntf2_1_inst.io.s <= commonSelt @[SDC_SDF_combFFT.scala 217:15]
    stageIntf2[1] <= stageIntf2_1_inst.io.out @[SDC_SDF_combFFT.scala 301:23]
    reg stageIntf1_1_REG : Fixed<<16>>, clock @[SDC_SDF_combFFT.scala 302:33]
    stageIntf1_1_REG <= inst_2.io.A @[SDC_SDF_combFFT.scala 302:33]
    stageIntf1[1] <= stageIntf1_1_REG @[SDC_SDF_combFFT.scala 302:23]
    node _postStgSelt_T = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 305:58]
    node _postStgSelt_T_1 = bits(_postStgSelt_T, 0, 0) @[SDC_SDF_combFFT.scala 305:68]
    node _postStgSelt_T_2 = eq(_postStgSelt_T_1, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 305:51]
    node _postStgSelt_T_3 = bits(dCount, 0, 0) @[SDC_SDF_combFFT.scala 305:78]
    node _postStgSelt_T_4 = bits(_postStgSelt_T_3, 0, 0) @[SDC_SDF_combFFT.scala 305:88]
    node postStgSelt = mux(UInt<1>("h00"), _postStgSelt_T_2, _postStgSelt_T_4) @[SDC_SDF_combFFT.scala 305:24]
    inst inst_6 of Commutator_3 @[SDC_SDF_combFFT.scala 90:22]
    inst_6.clock <= clock
    inst_6.reset <= reset
    inst_6.io.in1 <= stageIntf1[3] @[SDC_SDF_combFFT.scala 91:17]
    inst_6.io.in2 <= stageIntf2[3] @[SDC_SDF_combFFT.scala 92:17]
    inst_6.io.s <= postStgSelt @[SDC_SDF_combFFT.scala 93:15]
    wire wireFpToComp : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[SDC_SDF_combFFT.scala 308:26]
    wireFpToComp.re <= inst_6.io.out1 @[SDC_SDF_combFFT.scala 309:19]
    wireFpToComp.im <= inst_6.io.out2 @[SDC_SDF_combFFT.scala 310:19]
    wire sdfSelect : UInt<1> @[SDC_SDF_combFFT.scala 312:23]
    node _T_10 = geq(dCount, UInt<5>("h013")) @[SDC_SDF_combFFT.scala 313:16]
    when _T_10 : @[SDC_SDF_combFFT.scala 313:47]
      sdfSelect <= UInt<1>("h01") @[SDC_SDF_combFFT.scala 314:15]
      skip @[SDC_SDF_combFFT.scala 313:47]
    else : @[SDC_SDF_combFFT.scala 315:5]
      sdfSelect <= UInt<1>("h00") @[SDC_SDF_combFFT.scala 316:15]
      skip @[SDC_SDF_combFFT.scala 315:5]
    inst sdfReturn_inst of SDFUnit @[SDC_SDF_combFFT.scala 116:22]
    sdfReturn_inst.clock <= clock
    sdfReturn_inst.reset <= reset
    sdfReturn_inst.io.in.im <= wireFpToComp.im @[SDC_SDF_combFFT.scala 117:16]
    sdfReturn_inst.io.in.re <= wireFpToComp.re @[SDC_SDF_combFFT.scala 117:16]
    sdfReturn_inst.io.ctrl <= sdfSelect @[SDC_SDF_combFFT.scala 118:18]
    io.dataOut.re <= inst_1.io.out1 @[SDC_SDF_combFFT.scala 321:17]
    io.dataOut.im <= inst_1.io.out2 @[SDC_SDF_combFFT.scala 322:17]
    reg io_outValid_REG : UInt, clock @[SDC_SDF_combFFT.scala 323:25]
    io_outValid_REG <= dCount @[SDC_SDF_combFFT.scala 323:25]
    node _io_outValid_T = eq(io_outValid_REG, UInt<6>("h022")) @[SDC_SDF_combFFT.scala 323:34]
    io.outValid <= _io_outValid_T @[SDC_SDF_combFFT.scala 323:15]
    
