package jcl.compiler.real.icg;

import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.structs.numbers.RealStruct;

public class ComplexCodeGenerator {

	public static void genCodeComplex(final IntermediateCodeGenerator icg, final ComplexStruct complexStruct) {
		// TODO: we NEED to do Complex numbers better!!
		final RealStruct real = complexStruct.getReal();
		final RealStruct imaginary = complexStruct.getImaginary();

		if (real instanceof IntegerStruct) {
			IntegerCodeGenerator.genCodeInteger(icg, (IntegerStruct) real);
		} else if (real instanceof FloatStruct) {
			FloatCodeGenerator.genCodeFloat(icg, (FloatStruct) real);
		} else if (real instanceof RatioStruct) {
			RatioCodeGenerator.genCodeRatio(icg, (RatioStruct) real);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerStruct) {
			IntegerCodeGenerator.genCodeInteger(icg, (IntegerStruct) imaginary);
		} else if (imaginary instanceof FloatStruct) {
			FloatCodeGenerator.genCodeFloat(icg, (FloatStruct) imaginary);
		} else if (imaginary instanceof RatioStruct) {
			RatioCodeGenerator.genCodeRatio(icg, (RatioStruct) imaginary);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		icg.emitter.emitInvokestatic("jcl/structs/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)", "Ljcl/numbers/ComplexStruc;", false);
	}
}
