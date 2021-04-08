;buildInfoPackage: chisel3, version: 3.4.2, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit Reorder : 
  module Reorder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, test : UInt<4>, flip inValid : UInt<1>, outValid : UInt<1>}
    
    cmem ram : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[16] @[SDC_SDF_combFFT.scala 260:16]
    reg inCounter : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[SDC_SDF_combFFT.scala 262:26]
    reg inCounterCut : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[SDC_SDF_combFFT.scala 263:29]
    reg outCounter : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[SDC_SDF_combFFT.scala 264:27]
    node _index0_T = bits(inCounterCut, 1, 0) @[Bitwise.scala 109:18]
    node index0_hi = bits(_index0_T, 0, 0) @[Bitwise.scala 109:18]
    node index0_lo = bits(_index0_T, 1, 1) @[Bitwise.scala 109:44]
    node index0_hi_1 = cat(index0_hi, index0_lo) @[Cat.scala 30:58]
    node index0_lo_1 = bits(inCounterCut, 2, 2) @[Bitwise.scala 109:44]
    node index0 = cat(index0_hi_1, index0_lo_1) @[Cat.scala 30:58]
    node _index1_T = bits(inCounterCut, 1, 0) @[Bitwise.scala 109:18]
    node index1_hi = bits(_index1_T, 0, 0) @[Bitwise.scala 109:18]
    node index1_lo = bits(_index1_T, 1, 1) @[Bitwise.scala 109:44]
    node index1_hi_1 = cat(index1_hi, index1_lo) @[Cat.scala 30:58]
    node index1_lo_1 = bits(inCounterCut, 2, 2) @[Bitwise.scala 109:44]
    node _index1_T_1 = cat(index1_hi_1, index1_lo_1) @[Cat.scala 30:58]
    node _index1_T_2 = add(_index1_T_1, UInt<4>("h08")) @[SDC_SDF_combFFT.scala 266:38]
    node index1 = tail(_index1_T_2, 1) @[SDC_SDF_combFFT.scala 266:38]
    node _T = neq(inCounter, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 267:33]
    node _T_1 = or(io.inValid, _T) @[SDC_SDF_combFFT.scala 267:20]
    when _T_1 : @[SDC_SDF_combFFT.scala 267:42]
      node _T_2 = lt(inCounter, UInt<4>("h08")) @[SDC_SDF_combFFT.scala 268:21]
      when _T_2 : @[SDC_SDF_combFFT.scala 268:42]
        write mport MPORT = ram[index1], clock
        MPORT.im <= io.in.im
        MPORT.re <= io.in.re
        skip @[SDC_SDF_combFFT.scala 268:42]
      else : @[SDC_SDF_combFFT.scala 270:7]
        write mport MPORT_1 = ram[index0], clock
        MPORT_1.im <= io.in.im
        MPORT_1.re <= io.in.re
        skip @[SDC_SDF_combFFT.scala 270:7]
      node _inCounter_T = add(inCounter, UInt<1>("h01")) @[SDC_SDF_combFFT.scala 273:28]
      node _inCounter_T_1 = tail(_inCounter_T, 1) @[SDC_SDF_combFFT.scala 273:28]
      inCounter <= _inCounter_T_1 @[SDC_SDF_combFFT.scala 273:15]
      node _inCounterCut_T = add(inCounterCut, UInt<1>("h01")) @[SDC_SDF_combFFT.scala 274:34]
      node _inCounterCut_T_1 = tail(_inCounterCut_T, 1) @[SDC_SDF_combFFT.scala 274:34]
      inCounterCut <= _inCounterCut_T_1 @[SDC_SDF_combFFT.scala 274:18]
      skip @[SDC_SDF_combFFT.scala 267:42]
    reg io_outValid_REG : UInt, clock @[SDC_SDF_combFFT.scala 276:26]
    io_outValid_REG <= inCounter @[SDC_SDF_combFFT.scala 276:26]
    node _io_outValid_T = eq(io_outValid_REG, UInt<4>("h0f")) @[SDC_SDF_combFFT.scala 276:38]
    node _io_outValid_T_1 = neq(outCounter, UInt<1>("h00")) @[SDC_SDF_combFFT.scala 276:76]
    node _io_outValid_T_2 = or(_io_outValid_T, _io_outValid_T_1) @[SDC_SDF_combFFT.scala 276:61]
    io.outValid <= _io_outValid_T_2 @[SDC_SDF_combFFT.scala 276:15]
    when io.outValid : @[SDC_SDF_combFFT.scala 277:22]
      node _outCounter_T = add(outCounter, UInt<1>("h01")) @[SDC_SDF_combFFT.scala 278:30]
      node _outCounter_T_1 = tail(_outCounter_T, 1) @[SDC_SDF_combFFT.scala 278:30]
      outCounter <= _outCounter_T_1 @[SDC_SDF_combFFT.scala 278:16]
      skip @[SDC_SDF_combFFT.scala 277:22]
    read mport io_out_MPORT = ram[outCounter], clock @[SDC_SDF_combFFT.scala 280:21]
    io.out.im <= io_out_MPORT.im @[SDC_SDF_combFFT.scala 280:10]
    io.out.re <= io_out_MPORT.re @[SDC_SDF_combFFT.scala 280:10]
    io.test <= index0 @[SDC_SDF_combFFT.scala 281:11]
    
