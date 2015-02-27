package jcl.compiler.real.icg;

import jcl.compiler.real.element.RatioElement;

public class RatioCodeGenerator implements CodeGenerator<RatioElement> {

	public static final RatioCodeGenerator INSTANCE = new RatioCodeGenerator();

	@Override
	public void generate(final RatioElement input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitLdc(input.getBigFraction().getNumerator().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitLdc(input.getBigFraction().getDenominator().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitInvokestatic("jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)", "V", false);
	}
}
