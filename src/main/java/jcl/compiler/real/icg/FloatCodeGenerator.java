package jcl.compiler.real.icg;

import jcl.compiler.real.element.FloatElement;

public class FloatCodeGenerator implements CodeGenerator<FloatElement> {

	public static final FloatCodeGenerator INSTANCE = new FloatCodeGenerator();

	@Override
	public void generate(final FloatElement input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitLdc(input.getBigDecimal().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigDecimal", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitInvokestatic("jcl/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)", "V", false);
	}
}
