;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit RealAdder : 
  module RealAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, flip s : UInt<1>, out : Fixed<32><<16>>}
    
    when io.s : @[SDC_SDF_combFFT.scala 199:15]
      node _io_out_T = add(io.in1, io.in2) @[SDC_SDF_combFFT.scala 200:22]
      node _io_out_T_1 = tail(_io_out_T, 1) @[SDC_SDF_combFFT.scala 200:22]
      node _io_out_T_2 = asFixedPoint(_io_out_T_1, 16) @[SDC_SDF_combFFT.scala 200:22]
      io.out <= _io_out_T_2 @[SDC_SDF_combFFT.scala 200:12]
      skip @[SDC_SDF_combFFT.scala 199:15]
    else : @[SDC_SDF_combFFT.scala 201:5]
      node _io_out_T_3 = sub(io.in1, io.in2) @[SDC_SDF_combFFT.scala 202:22]
      node _io_out_T_4 = tail(_io_out_T_3, 1) @[SDC_SDF_combFFT.scala 202:22]
      node _io_out_T_5 = asFixedPoint(_io_out_T_4, 16) @[SDC_SDF_combFFT.scala 202:22]
      io.out <= _io_out_T_5 @[SDC_SDF_combFFT.scala 202:12]
      skip @[SDC_SDF_combFFT.scala 201:5]
    
