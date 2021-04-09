import chisel3.experimental.FixedPoint
import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import chiseltest.ChiselScalatestTester
import edu.ucsb.jinhuali.combFFT.{CombFFT, Commutator, HasConfig, RealAddSub, RealAdder, RealBranchMul, Reorder, SDFUnit, Switch}
import org.scalatest.{FlatSpec, Matchers}

class BasicTest extends FlatSpec with ChiselScalatestTester with Matchers with HasConfig {
  behavior of "Commutator"
  it should "commute numbers" in {
    test(new Commutator(2)) { c =>
      var first = true
      for (i <- 0 until 6) {
        if (first) {first = false} else {c.clock.step(1)}
        c.io.s.poke(false.B)
        c.io.in1.poke(FixedPoint.fromDouble(0.0+i*4.0, dataWidth.W, binaryPoint.BP))
        c.io.in2.poke(FixedPoint.fromDouble(-0.0-i*4.0, dataWidth.W, binaryPoint.BP))
        println(s"(${c.io.out1.peek().toString()}, ${c.io.out2.peek().toString()})")
        c.clock.step(1)
        c.io.s.poke(false.B)
        c.io.in1.poke(FixedPoint.fromDouble(1.0+i*4.0, dataWidth.W, binaryPoint.BP))
        c.io.in2.poke(FixedPoint.fromDouble(-1.0-i*4.0, dataWidth.W, binaryPoint.BP))
        println(s"(${c.io.out1.peek().toString()}, ${c.io.out2.peek().toString()})")
        c.clock.step(1)
        c.io.s.poke(true.B)
        c.io.in1.poke(FixedPoint.fromDouble(2.0+i*4.0, dataWidth.W, binaryPoint.BP))
        c.io.in2.poke(FixedPoint.fromDouble(-2.0-i*4.0, dataWidth.W, binaryPoint.BP))
        println(s"(${c.io.out1.peek().toString()}, ${c.io.out2.peek().toString()})")
        c.clock.step(1)
        c.io.s.poke(true.B)
        c.io.in1.poke(FixedPoint.fromDouble(3.0+i*4.0, dataWidth.W, binaryPoint.BP))
        c.io.in2.poke(FixedPoint.fromDouble(-3.0-i*4.0, dataWidth.W, binaryPoint.BP))
        println(s"(${c.io.out1.peek().toString()}, ${c.io.out2.peek().toString()})")
      }
    }
  }

  behavior of "SDFUnit"
  it should "sdf" in {
    test (new SDFUnit) { c =>
      for (j <- 0 to 1) {
        for (i <- 0 to 14 by 2) {
          c.clock.step(1)
          c.io.in.re.poke(FixedPoint.fromDouble(0.0+j+i, dataWidth.W, binaryPoint.BP))
          c.io.in.im.poke(FixedPoint.fromDouble(-(0.0+j+i), dataWidth.W, binaryPoint.BP))
          println((if (j == 0) "invalid data:" else "") + c.io.out.peek())
        }
      }
      for (i <- 0 to 7) {
        c.clock.step(1)
        c.io.in.re.poke(FixedPoint.fromDouble(0.0, dataWidth.W, binaryPoint.BP))
        c.io.in.im.poke(FixedPoint.fromDouble(0.0, dataWidth.W, binaryPoint.BP))
        println(c.io.out.peek())
      }
    }
  }

  behavior of "RealAddSub"
  it should "add and sub" in {
    test (new RealAddSub) { c =>
      c.io.a.poke(FixedPoint.fromDouble(3.0, dataWidth.W, binaryPoint.BP))
      c.io.b.poke(FixedPoint.fromDouble(5.0, dataWidth.W, binaryPoint.BP))
      println(s"${c.io.A.peek()}, ${c.io.B.peek()}")
    }
  }

  behavior of "Switch"
  it should "switch data or not" in {
    test (new Switch) { c =>
      c.io.in1.poke(FixedPoint.fromDouble(1.0, dataWidth.W, binaryPoint.BP))
      c.io.in2.poke(FixedPoint.fromDouble(2.0, dataWidth.W, binaryPoint.BP))
      c.io.s.poke(true.B)
      println(s"${c.io.out1.peek()}, ${c.io.out2.peek()}")
      c.io.s.poke(false.B)
      println(s"${c.io.out1.peek()}, ${c.io.out2.peek()}")
    }
  }

  behavior of "RealBranchMult"
  it should "branch multiply" in {
    test (new RealBranchMul) { c =>
      c.io.dataIn.poke(FixedPoint.fromDouble(2.0, dataWidth.W, binaryPoint.BP))
      c.io.w_re_in.poke(FixedPoint.fromDouble(3.0, dataWidth.W, binaryPoint.BP))
      c.io.w_im_in.poke(FixedPoint.fromDouble(4.0, dataWidth.W, binaryPoint.BP))
      println(s"${c.io.out1.peek()}, ${c.io.out2.peek()}")
    }
  }

  behavior of "RealAdder"
  it should "add/sub by select" in {
    test (new RealAdder) { c =>
      c.io.in1.poke(FixedPoint.fromDouble(2.0, dataWidth.W, binaryPoint.BP))
      c.io.in2.poke(FixedPoint.fromDouble(3.0, dataWidth.W, binaryPoint.BP))
      c.io.s.poke(true.B)
      println(c.io.out.peek())
      c.io.s.poke(false.B)
      println(c.io.out.peek())
    }
  }

  behavior of "Reorder"
  it should "reorder data" in {
    test (new Reorder) { c =>
      for (i <- 0 until FFTLength) {
        if (i == 0) c.io.inValid.poke(true.B) else c.io.inValid.poke(false.B)
        c.io.in.re.poke(FixedPoint.fromDouble(0.0+i, dataWidth.W, binaryPoint.BP))
        c.io.in.im.poke(FixedPoint.fromDouble(0.0-i, dataWidth.W, binaryPoint.BP))
        c.clock.step(1)
        // println(c.io.test.peek())
      }
      for (i <- 0 until FFTLength) {
        println(c.io.out.peek())
        c.clock.step(1)
      }
    }
  }
}

class Complex(val re: Double, val im: Double) {
  def +(rhs: Complex): Complex = new Complex(re + rhs.re, im + rhs.im)
  def -(rhs: Complex): Complex = new Complex(re - rhs.re, im - rhs.im)
  def *(rhs: Complex): Complex = new Complex(re * rhs.re - im * rhs.im, rhs.re * im + re * rhs.im)
  def magnitude: Double = Math.hypot(re, im)
  def phase: Double = Math.atan2(im, re)
  override def toString: String = s"Complex($re, $im)"
}

class FFTTest extends FlatSpec with ChiselScalatestTester with Matchers
  with HasConfig {
  behavior of "CombFFT"
  it should "perform FFT" in {
    test (new CombFFT) { c =>

      def fft(x: Array[Complex]): Array[Complex] = {
        require(x.length > 0 && (x.length & (x.length - 1)) == 0, "array size should be power of two")
        fft__(x, 0, x.length, 1)
      }

      def fft__(x: Array[Complex], start: Int, n: Int, stride: Int) : Array[Complex] = {
        if (n == 1) {
          return Array(x(start))
        }
        val X = fft__(x, start, n / 2, 2 * stride) ++ fft__(x, start + stride, n / 2, 2 * stride)
        for (k <- 0 until n / 2) {
          val t = X(k)
          val arg = -2 * math.Pi * k / n
          val c = new Complex(math.cos(arg), math.sin(arg)) * X(k + n / 2)
          X(k) = t + c
          X(k + n / 2) = t - c
        }
        X
      }

      // check if FFT is 2^k
      val log2N: Double = math.log(FFTLength)/math.log(2)
      require(math.round(log2N) == log2N)
      val r = new scala.util.Random
      val bound: Double = math.pow(2.0, 10)
      var error: Double = 0
      var ovNum: Int = 0
      var iterNum: Int = 10

      for (i <- 0 until iterNum) {
        val a = new Array[Complex](FFTLength)
        var cnt = 0
        for (i <- 0 until FFTLength) {
          val re = -bound.toInt / 2 + r.nextInt(bound.toInt)
          val im = -bound.toInt / 2 + r.nextInt(bound.toInt)
          a(cnt) = new Complex(2 * re / bound, 2 * im / bound)
          c.io.dataIn.re.poke(FixedPoint.fromDouble(re.toDouble, dataWidth.W, binaryPoint.BP))
          c.io.dataIn.im.poke(FixedPoint.fromDouble(im.toDouble, dataWidth.W, binaryPoint.BP))
          if (i == 0) {
            c.io.inValid.poke(true.B)
          } else {
            c.io.inValid.poke(false.B)
          }
          c.clock.step(1)
          cnt += 1
        }
        val ref = fft(a)
        // wait for data to come out
        c.clock.step((1.5*FFTLength+log2Ceil(FFTLength)-1).toInt)
        var errorOne: Double = 0
        var error1: Double = 0
        var ovNum1: Int = 0
        val eps: Double = 1e-9
        for (i <- 0 until FFTLength) {
          val ref1 = ref(i)
          val data = c.io.dataOut.peek()
          error1 = 0.5 * (math.abs(((2 * data.re.litToDouble / bound) - ref1.re) / (ref1.re + eps)) + math.abs(((2 * data.im.litToDouble / bound) - ref1.im) / (ref1.im + eps)))
          if (error1 <= 0.1) {
            errorOne += error1
          } else {
            ovNum1 += 1
          }
          c.clock.step(1)
        }
        errorOne = errorOne / (FFTLength - ovNum1)
        ovNum += ovNum1
        error += errorOne
        val errorOnePercent = errorOne * 100
        println(f"In this sample, error rate is : $errorOnePercent%.2f%% | number of overflows is: $ovNum1%d\n")
      }
      println()
      error /= iterNum
      print("Total error rate is: " + error*100 + "%\n")
      print(ovNum + " of " + iterNum * FFTLength + " overflowed! " + "The overflow ratio is " + 100 * ovNum / (FFTLength * iterNum).toDouble  + "%" + "\n")
    }
  }
}
