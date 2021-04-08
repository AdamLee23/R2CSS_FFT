;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit RealAddSub : 
  module RealAddSub : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : Fixed<32><<16>>, flip b : Fixed<32><<16>>, A : Fixed<32><<16>>, B : Fixed<32><<16>>}
    
    node _io_A_T = add(io.a, io.b) @[SDC_SDF_combFFT.scala 125:16]
    node _io_A_T_1 = tail(_io_A_T, 1) @[SDC_SDF_combFFT.scala 125:16]
    node _io_A_T_2 = asFixedPoint(_io_A_T_1, 16) @[SDC_SDF_combFFT.scala 125:16]
    io.A <= _io_A_T_2 @[SDC_SDF_combFFT.scala 125:8]
    node _io_B_T = sub(io.a, io.b) @[SDC_SDF_combFFT.scala 126:16]
    node _io_B_T_1 = tail(_io_B_T, 1) @[SDC_SDF_combFFT.scala 126:16]
    node _io_B_T_2 = asFixedPoint(_io_B_T_1, 16) @[SDC_SDF_combFFT.scala 126:16]
    io.B <= _io_B_T_2 @[SDC_SDF_combFFT.scala 126:8]
    
