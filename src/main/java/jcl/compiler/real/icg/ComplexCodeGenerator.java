package jcl.compiler.real.icg;

import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.structs.numbers.RealStruct;

public class ComplexCodeGenerator implements CodeGenerator<ComplexStruct> {

	public static final ComplexCodeGenerator INSTANCE = new ComplexCodeGenerator();

	@Override
	public void generate(final ComplexStruct input, final IntermediateCodeGenerator codeGenerator) {
		// TODO: we NEED to do Complex numbers better!!
		final RealStruct real = input.getReal();
		final RealStruct imaginary = input.getImaginary();

		if (real instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) real, codeGenerator);
		} else if (real instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) real, codeGenerator);
		} else if (real instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) real, codeGenerator);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) imaginary, codeGenerator);
		} else if (imaginary instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) imaginary, codeGenerator);
		} else if (imaginary instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) imaginary, codeGenerator);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		codeGenerator.emitter.emitInvokestatic("jcl/structs/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)", "Ljcl/numbers/ComplexStruc;", false);
	}
}
