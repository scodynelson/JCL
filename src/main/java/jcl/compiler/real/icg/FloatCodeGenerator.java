package jcl.compiler.real.icg;

import jcl.numbers.FloatStruct;
import org.springframework.stereotype.Component;

@Component
public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	@Override
	public void generate(final FloatStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitLdc(input.getBigDecimal().toString());
		classBuilder.getEmitter().emitInvokestatic("java/math/BigDecimal", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitInvokestatic("jcl/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)", "V", false);
	}
}
