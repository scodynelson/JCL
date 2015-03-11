package jcl.compiler.real.icg;

import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	@Override
	public void generate(final IntegerStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitLdc(input.getBigInteger().toString());
		classBuilder.getEmitter().emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitInvokestatic("jcl/numbers/IntegerStruct", "<init>", "(Ljava/math/BigInteger;)", "V", false);
	}
}
