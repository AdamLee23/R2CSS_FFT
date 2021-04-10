circuit Reorder :
  module Reorder :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>
    input io_inValid : UInt<1>
    output io_outValid : UInt<1>

    mem ram_re : @[SDC_SDF_combFFT.scala 246:16]
      data-type => SInt<32>
      depth => 16
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      read-under-write => undefined
    mem ram_im : @[SDC_SDF_combFFT.scala 246:16]
      data-type => SInt<32>
      depth => 16
      read-latency => 0
      write-latency => 1
      reader => io_out_MPORT
      writer => MPORT
      read-under-write => undefined
    reg inCounter : UInt<3>, clock with :
      reset => (UInt<1>("h0"), inCounter) @[SDC_SDF_combFFT.scala 248:26]
    reg ICNextBit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ICNextBit) @[SDC_SDF_combFFT.scala 249:26]
    reg outCounter : UInt<4>, clock with :
      reset => (UInt<1>("h0"), outCounter) @[SDC_SDF_combFFT.scala 250:27]
    node _index0_T = bits(inCounter, 1, 0) @[Bitwise.scala 109:18]
    node index0_hi = bits(_index0_T, 0, 0) @[Bitwise.scala 109:18]
    node index0_lo = bits(_index0_T, 1, 1) @[Bitwise.scala 109:44]
    node index0_hi_1 = cat(index0_hi, index0_lo) @[Cat.scala 30:58]
    node index0_lo_1 = bits(inCounter, 2, 2) @[Bitwise.scala 109:44]
    node index0 = cat(index0_hi_1, index0_lo_1) @[Cat.scala 30:58]
    node _index1_T = bits(inCounter, 1, 0) @[Bitwise.scala 109:18]
    node index1_hi = bits(_index1_T, 0, 0) @[Bitwise.scala 109:18]
    node index1_lo = bits(_index1_T, 1, 1) @[Bitwise.scala 109:44]
    node index1_hi_1 = cat(index1_hi, index1_lo) @[Cat.scala 30:58]
    node index1_lo_1 = bits(inCounter, 2, 2) @[Bitwise.scala 109:44]
    node _index1_T_1 = cat(index1_hi_1, index1_lo_1) @[Cat.scala 30:58]
    node _index1_T_2 = add(_index1_T_1, UInt<4>("h8")) @[SDC_SDF_combFFT.scala 252:35]
    node index1 = tail(_index1_T_2, 1) @[SDC_SDF_combFFT.scala 252:35]
    node _T = neq(inCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:34]
    node _T_1 = neq(ICNextBit, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:55]
    node _T_2 = or(_T, _T_1) @[SDC_SDF_combFFT.scala 253:42]
    node _T_3 = or(io_inValid, _T_2) @[SDC_SDF_combFFT.scala 253:20]
    node _T_4 = eq(ICNextBit, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 254:29]
    node _T_5 = mux(_T_4, index1, index0) @[SDC_SDF_combFFT.scala 254:18]
    node _inCounter_T = add(inCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 255:28]
    node _inCounter_T_1 = tail(_inCounter_T, 1) @[SDC_SDF_combFFT.scala 255:28]
    node _T_6 = eq(inCounter, UInt<3>("h7")) @[SDC_SDF_combFFT.scala 256:21]
    node _ICNextBit_T = add(ICNextBit, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 257:30]
    node _ICNextBit_T_1 = tail(_ICNextBit_T, 1) @[SDC_SDF_combFFT.scala 257:30]
    node _GEN_0 = mux(_T_6, _ICNextBit_T_1, ICNextBit) @[SDC_SDF_combFFT.scala 256:48 SDC_SDF_combFFT.scala 257:17 SDC_SDF_combFFT.scala 249:26]
    node _GEN_1 = validif(_T_3, _T_5) @[SDC_SDF_combFFT.scala 253:65]
    node _GEN_2 = validif(_T_3, clock) @[SDC_SDF_combFFT.scala 253:65]
    node _GEN_3 = mux(_T_3, UInt<1>("h1"), UInt<1>("h0")) @[SDC_SDF_combFFT.scala 253:65 SDC_SDF_combFFT.scala 246:16]
    node _GEN_4 = validif(_T_3, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 253:65]
    node _GEN_5 = validif(_T_3, io_in_im) @[SDC_SDF_combFFT.scala 253:65]
    node _GEN_6 = validif(_T_3, io_in_re) @[SDC_SDF_combFFT.scala 253:65]
    node _GEN_7 = mux(_T_3, _inCounter_T_1, inCounter) @[SDC_SDF_combFFT.scala 253:65 SDC_SDF_combFFT.scala 255:15 SDC_SDF_combFFT.scala 248:26]
    node _GEN_8 = mux(_T_3, _GEN_0, ICNextBit) @[SDC_SDF_combFFT.scala 253:65 SDC_SDF_combFFT.scala 249:26]
    node _io_outValid_T = cat(ICNextBit, inCounter) @[Cat.scala 30:58]
    reg io_outValid_REG : UInt<4>, clock with :
      reset => (UInt<1>("h0"), io_outValid_REG) @[SDC_SDF_combFFT.scala 260:26]
    node _io_outValid_T_1 = eq(io_outValid_REG, UInt<4>("hf")) @[SDC_SDF_combFFT.scala 260:54]
    node _io_outValid_T_2 = neq(outCounter, UInt<1>("h0")) @[SDC_SDF_combFFT.scala 260:92]
    node _io_outValid_T_3 = or(_io_outValid_T_1, _io_outValid_T_2) @[SDC_SDF_combFFT.scala 260:77]
    node _outCounter_T = add(outCounter, UInt<1>("h1")) @[SDC_SDF_combFFT.scala 262:30]
    node _outCounter_T_1 = tail(_outCounter_T, 1) @[SDC_SDF_combFFT.scala 262:30]
    node _GEN_9 = mux(io_outValid, _outCounter_T_1, outCounter) @[SDC_SDF_combFFT.scala 261:22 SDC_SDF_combFFT.scala 262:16 SDC_SDF_combFFT.scala 250:27]
    io_out_re <= ram_re.io_out_MPORT.data @[SDC_SDF_combFFT.scala 264:10]
    io_out_im <= ram_im.io_out_MPORT.data @[SDC_SDF_combFFT.scala 264:10]
    io_outValid <= _io_outValid_T_3 @[SDC_SDF_combFFT.scala 260:15]
    ram_re.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 264:21]
    ram_im.io_out_MPORT.addr <= outCounter @[SDC_SDF_combFFT.scala 264:21]
    ram_re.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 264:21]
    ram_im.io_out_MPORT.en <= UInt<1>("h1") @[SDC_SDF_combFFT.scala 264:21]
    ram_re.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 264:21]
    ram_im.io_out_MPORT.clk <= clock @[SDC_SDF_combFFT.scala 264:21]
    ram_re.MPORT.addr <= _GEN_1
    ram_im.MPORT.addr <= _GEN_1
    ram_re.MPORT.en <= _GEN_3
    ram_im.MPORT.en <= _GEN_3
    ram_re.MPORT.clk <= _GEN_2
    ram_im.MPORT.clk <= _GEN_2
    ram_re.MPORT.data <= _GEN_6
    ram_im.MPORT.data <= _GEN_5
    ram_re.MPORT.mask <= _GEN_4
    ram_im.MPORT.mask <= _GEN_4
    inCounter <= mux(reset, UInt<3>("h0"), _GEN_7) @[SDC_SDF_combFFT.scala 248:26 SDC_SDF_combFFT.scala 248:26]
    ICNextBit <= mux(reset, UInt<1>("h0"), _GEN_8) @[SDC_SDF_combFFT.scala 249:26 SDC_SDF_combFFT.scala 249:26]
    outCounter <= mux(reset, UInt<4>("h0"), _GEN_9) @[SDC_SDF_combFFT.scala 250:27 SDC_SDF_combFFT.scala 250:27]
    io_outValid_REG <= _io_outValid_T @[SDC_SDF_combFFT.scala 260:26]
