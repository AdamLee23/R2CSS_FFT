circuit Reorder :
  module Reorder :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>
    output io_test : UInt<4>
    input io_inValid : UInt<1>
    output io_outValid : UInt<1>

    mem ram_re : @[SDC_SDF_combFFT.scala 260:16]
      data-type => SInt<32>
      depth => 16
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      writer => MPORT_1
      read-under-write => undefined
    mem ram_im : @[SDC_SDF_combFFT.scala 260:16]
      data-type => SInt<32>
      depth => 16
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      writer => MPORT_1
      read-under-write => undefined
    reg inCounter : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inCounter) @[SDC_SDF_combFFT.scala 262:26]
    reg inCounterCut : UInt<3>, clock with :
      reset => (UInt<1>("h0"), inCounterCut) @[SDC_SDF_combFFT.scala 263:29]
    reg outCounter : UInt<4>, clock with :
      reset => (UInt<1>("h0"), outCounter) @[SDC_SDF_combFFT.scala 264:27]
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
    node _index1_T_2 = add(_index1_T_1, UInt<4>("h8")) @[SDC_SDF_combFFT.scala 266:38]
    node index1 = tail(_index1_T_2, 1) @[SDC_SDF_combFFT.scala 266:38]
    node _T = neq(inCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 267:33]
    node _T_1 = or(io_inValid, _T) @[SDC_SDF_combFFT.scala 267:20]
    node _T_2 = lt(inCounter, UInt<4>("h8")) @[SDC_SDF_combFFT.scala 268:21]
    node _GEN_0 = validif(_T_2, index1) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_1 = validif(_T_2, clock) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_2 = mux(_T_2, UInt<1>("h1"), UInt<1>("h0")) @[SDC_SDF_combFFT.scala 268:42 SDC_SDF_combFFT.scala 260:16]
    node _GEN_3 = validif(_T_2, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_4 = validif(_T_2, io_in_im) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_5 = validif(_T_2, io_in_re) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_6 = validif(eq(_T_2, UInt<1>("h0")), index0) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_7 = validif(eq(_T_2, UInt<1>("h0")), clock) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_8 = mux(_T_2, UInt<1>("h0"), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 268:42 SDC_SDF_combFFT.scala 260:16]
    node _GEN_9 = validif(eq(_T_2, UInt<1>("h0")), UInt<1>("h1")) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_10 = validif(eq(_T_2, UInt<1>("h0")), io_in_im) @[SDC_SDF_combFFT.scala 268:42]
    node _GEN_11 = validif(eq(_T_2, UInt<1>("h0")), io_in_re) @[SDC_SDF_combFFT.scala 268:42]
    node _inCounter_T = add(inCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 273:28]
    node _inCounter_T_1 = tail(_inCounter_T, 1) @[SDC_SDF_combFFT.scala 273:28]
    node _inCounterCut_T = add(inCounterCut, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 274:34]
    node _inCounterCut_T_1 = tail(_inCounterCut_T, 1) @[SDC_SDF_combFFT.scala 274:34]
    node _GEN_12 = validif(_T_1, _GEN_0) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_13 = validif(_T_1, _GEN_1) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_14 = mux(_T_1, _GEN_2, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 267:42 SDC_SDF_combFFT.scala 260:16]
    node _GEN_15 = validif(_T_1, _GEN_3) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_16 = validif(_T_1, _GEN_4) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_17 = validif(_T_1, _GEN_5) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_18 = validif(_T_1, _GEN_6) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_19 = validif(_T_1, _GEN_7) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_20 = mux(_T_1, _GEN_8, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 267:42 SDC_SDF_combFFT.scala 260:16]
    node _GEN_21 = validif(_T_1, _GEN_9) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_22 = validif(_T_1, _GEN_10) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_23 = validif(_T_1, _GEN_11) @[SDC_SDF_combFFT.scala 267:42]
    node _GEN_24 = mux(_T_1, _inCounter_T_1, inCounter) @[SDC_SDF_combFFT.scala 267:42 SDC_SDF_combFFT.scala 273:15 SDC_SDF_combFFT.scala 262:26]
    node _GEN_25 = mux(_T_1, _inCounterCut_T_1, inCounterCut) @[SDC_SDF_combFFT.scala 267:42 SDC_SDF_combFFT.scala 274:18 SDC_SDF_combFFT.scala 263:29]
    reg io_outValid_REG : UInt<4>, clock with :
      reset => (UInt<1>("h0"), io_outValid_REG) @[SDC_SDF_combFFT.scala 276:26]
    node _io_outValid_T = eq(io_outValid_REG, UInt<4>("hf")) @[SDC_SDF_combFFT.scala 276:38]
    node _io_outValid_T_1 = neq(outCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 276:76]
    node _io_outValid_T_2 = or(_io_outValid_T, _io_outValid_T_1) @[SDC_SDF_combFFT.scala 276:61]
    node _outCounter_T = add(outCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 278:30]
    node _outCounter_T_1 = tail(_outCounter_T, 1) @[SDC_SDF_combFFT.scala 278:30]
    node _GEN_26 = mux(io_outValid, _outCounter_T_1, outCounter) @[SDC_SDF_combFFT.scala 277:22 SDC_SDF_combFFT.scala 278:16 SDC_SDF_combFFT.scala 264:27]
    io_out_re <= ram_re.io_out_MPORT.data @[SDC_SDF_combFFT.scala 280:10]
    io_out_im <= ram_im.io_out_MPORT.data @[SDC_SDF_combFFT.scala 280:10]
    io_test <= index0 @[SDC_SDF_combFFT.scala 281:11]
    io_outValid <= _io_outValid_T_2 @[SDC_SDF_combFFT.scala 276:15]
    ram_re.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 280:21]
    ram_im.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 280:21]
    ram_re.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 280:21]
    ram_im.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 280:21]
    ram_re.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 280:21]
    ram_im.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 280:21]
    ram_re.MPORT.addr <= _GEN_12
    ram_im.MPORT.addr <= _GEN_12
    ram_re.MPORT.en <= _GEN_14
    ram_im.MPORT.en <= _GEN_14
    ram_re.MPORT.clk <= _GEN_13
    ram_im.MPORT.clk <= _GEN_13
    ram_re.MPORT.data <= _GEN_17
    ram_im.MPORT.data <= _GEN_16
    ram_re.MPORT.mask <= _GEN_15
    ram_im.MPORT.mask <= _GEN_15
    ram_re.MPORT_1.addr <= _GEN_18
    ram_im.MPORT_1.addr <= _GEN_18
    ram_re.MPORT_1.en <= _GEN_20
    ram_im.MPORT_1.en <= _GEN_20
    ram_re.MPORT_1.clk <= _GEN_19
    ram_im.MPORT_1.clk <= _GEN_19
    ram_re.MPORT_1.data <= _GEN_23
    ram_im.MPORT_1.data <= _GEN_22
    ram_re.MPORT_1.mask <= _GEN_21
    ram_im.MPORT_1.mask <= _GEN_21
    inCounter <= mux(reset, UInt<4>("h0"), _GEN_24) @[SDC_SDF_combFFT.scala 262:26 SDC_SDF_combFFT.scala 262:26]
    inCounterCut <= mux(reset, UInt<3>("h0"), _GEN_25) @[SDC_SDF_combFFT.scala 263:29 SDC_SDF_combFFT.scala 263:29]
    outCounter <= mux(reset, UInt<4>("h0"), _GEN_26) @[SDC_SDF_combFFT.scala 264:27 SDC_SDF_combFFT.scala 264:27]
    io_outValid_REG <= inCounter @[SDC_SDF_combFFT.scala 276:26]
