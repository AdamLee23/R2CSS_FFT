circuit RealBranchMul :
  module RealBranchMul :
    input clock : Clock
    input reset : UInt<1>
    input io_dataIn : SInt<32>
    input io_w_re_in : SInt<32>
    input io_w_im_in : SInt<32>
    output io_out1 : SInt<32>
    output io_out2 : SInt<32>

    node _io_out1_T = mul(io_w_re_in, io_dataIn) @[SDC_SDF_combFFT.scala 176:25]
    node _io_out2_T = mul(io_w_im_in, io_dataIn) @[SDC_SDF_combFFT.scala 177:25]
    io_out1 <= asSInt(bits(shr(_io_out1_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 176:11]
    io_out2 <= asSInt(bits(shr(_io_out2_T, 16), 31, 0)) @[SDC_SDF_combFFT.scala 177:11]
