package jcl.compiler.real.icg.generator;

import java.math.BigDecimal;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	@Override
	public void generate(final FloatStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final BigDecimal bigDecimal = input.getBigDecimal();
		final String decimalString = bigDecimal.toString();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor methodVisitor = currentClass.getMethodVisitor();

		methodVisitor.visitLdcInsn(decimalString);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/math/BigDecimal", "<init>", "(Ljava/lang/String;)V", false);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)V", false);
	}
}
