package jcl.compiler.real.icg.generator.simple;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.RatioStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	@Override
	public void generate(final RatioStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final BigFraction bigFraction = input.getBigFraction();

		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger numerator = bigFraction.getNumerator();
		final String numeratorString = numerator.toString();
		mv.visitLdcInsn(numeratorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		final int numeratorStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, numeratorStore);

		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger denominator = bigFraction.getDenominator();
		final String denominatorString = denominator.toString();
		mv.visitLdcInsn(denominatorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		final int denominatorStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, denominatorStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/RatioStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, numeratorStore);
		mv.visitVarInsn(Opcodes.ALOAD, denominatorStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)V", false);
	}
}
