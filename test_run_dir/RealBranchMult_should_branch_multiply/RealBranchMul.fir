;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit RealBranchMul : 
  module RealBranchMul : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip dataIn : Fixed<32><<16>>, flip w_re_in : Fixed<32><<16>>, flip w_im_in : Fixed<32><<16>>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    node _io_out1_T = mul(io.w_re_in, io.dataIn) @[SDC_SDF_combFFT.scala 176:25]
    io.out1 <= _io_out1_T @[SDC_SDF_combFFT.scala 176:11]
    node _io_out2_T = mul(io.w_im_in, io.dataIn) @[SDC_SDF_combFFT.scala 177:25]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 177:11]
    
