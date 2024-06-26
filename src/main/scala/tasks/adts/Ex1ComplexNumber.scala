package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    private case class ComplexImpl(real: Double, imaginary: Double)
    opaque type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(real = re, imaginary = im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(real, _) => real
      def im(): Double = complex match
        case ComplexImpl(_, imaginary) => imaginary
      def sum(other: Complex): Complex =
        ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case ComplexImpl(re, im) if (re == 0 && im == 0) => re.toString
        case ComplexImpl(re, im) if (re != 0 && im == 0) => re.toString
        case ComplexImpl(re, im) if (re != 0 && im > 0) => re + " + " + im + "i"
        case ComplexImpl(re, im) if (re != 0 && im < 0) => re + " - " + Math.abs(im) + "i"
        case ComplexImpl(re, im) if (re == 0 && im > 0) => im + "i"
        case ComplexImpl(re, im) if (re == 0 && im < 0) => im + "i"

