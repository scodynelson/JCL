package jcl.compiler.real.icg.generator;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.numbers.RatioStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	@Override
	public void generate(final RatioStruct input, final JavaClassBuilder classBuilder) {

		final BigFraction bigFraction = input.getBigFraction();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor methodVisitor = currentClass.getMethodVisitor();

		final BigInteger numerator = bigFraction.getNumerator();
		final String numeratorString = numerator.toString();
		methodVisitor.visitLdcInsn(numeratorString);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);

		final BigInteger denominator = bigFraction.getDenominator();
		final String denominatorString = denominator.toString();
		methodVisitor.visitLdcInsn(denominatorString);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);

		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)V", false);
	}
}
