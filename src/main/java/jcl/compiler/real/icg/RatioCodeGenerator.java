package jcl.compiler.real.icg;

import jcl.numbers.RatioStruct;

public class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	public static final RatioCodeGenerator INSTANCE = new RatioCodeGenerator();

	@Override
	public void generate(final RatioStruct input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitLdc(input.getBigFraction().getNumerator().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitLdc(input.getBigFraction().getDenominator().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitInvokestatic("jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)", "V", false);
	}
}
