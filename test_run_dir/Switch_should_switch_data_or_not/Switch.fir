;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit Switch : 
  module Switch : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip s : UInt<1>, flip in1 : Fixed<32><<16>>, flip in2 : Fixed<32><<16>>, out1 : Fixed<32><<16>>, out2 : Fixed<32><<16>>}
    
    when io.s : @[SDC_SDF_combFFT.scala 148:15]
      io.out1 <= io.in1 @[SDC_SDF_combFFT.scala 149:13]
      io.out2 <= io.in2 @[SDC_SDF_combFFT.scala 150:13]
      skip @[SDC_SDF_combFFT.scala 148:15]
    else : @[SDC_SDF_combFFT.scala 151:5]
      io.out1 <= io.in2 @[SDC_SDF_combFFT.scala 152:13]
      io.out2 <= io.in1 @[SDC_SDF_combFFT.scala 153:13]
      skip @[SDC_SDF_combFFT.scala 151:5]
    
