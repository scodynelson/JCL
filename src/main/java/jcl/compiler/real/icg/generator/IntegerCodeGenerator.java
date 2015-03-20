package jcl.compiler.real.icg.generator;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	@Override
	public void generate(final IntegerStruct input, final JavaClassBuilder classBuilder) {

		final BigInteger bigInteger = input.getBigInteger();
		final String integerString = bigInteger.toString();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor methodVisitor = currentClass.getMethodVisitor();

		methodVisitor.visitLdcInsn(integerString);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/numbers/IntegerStruct", "<init>", "(Ljava/math/BigInteger;)V", false);
	}
}
