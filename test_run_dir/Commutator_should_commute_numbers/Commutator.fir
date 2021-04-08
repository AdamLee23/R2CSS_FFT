;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit Commutator : 
  module Commutator : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    wire wire1 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 108:19]
    wire wire2 : Fixed<32><<16>> @[SDC_SDF_combFFT.scala 109:19]
    reg wire1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r <= io.in2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg wire1_r_1 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      wire1_r_1 <= wire1_r @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    wire1 <= wire1_r_1 @[SDC_SDF_combFFT.scala 110:9]
    node _wire2_T = mux(io.s, wire1, io.in1) @[SDC_SDF_combFFT.scala 111:15]
    wire2 <= _wire2_T @[SDC_SDF_combFFT.scala 111:9]
    reg io_out1_r : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r <= wire2 @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg io_out1_r_1 : Fixed<32><<16>>, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      io_out1_r_1 <= io_out1_r @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out1 <= io_out1_r_1 @[SDC_SDF_combFFT.scala 112:11]
    node _io_out2_T = mux(io.s, io.in1, wire1) @[SDC_SDF_combFFT.scala 113:17]
    io.out2 <= _io_out2_T @[SDC_SDF_combFFT.scala 113:11]
    
