package jcl.compiler.real.icg.generator.simple;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.RatioStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	@Override
	public void generate(final RatioStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "ratioGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final BigFraction bigFraction = input.getBigFraction();

		final Label getNumerator = new Label();
		mv.visitLabel(getNumerator);
//		mv.visitLineNumber(119, getNumerator);
		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger numerator = bigFraction.getNumerator();
		final String numeratorString = numerator.toString();
		mv.visitLdcInsn(numeratorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label getDenominator = new Label();
		mv.visitLabel(getDenominator);
//		mv.visitLineNumber(120, getDenominator);
		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger denominator = bigFraction.getDenominator();
		final String denominatorString = denominator.toString();
		mv.visitLdcInsn(denominatorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label getRatioStruct = new Label();
		mv.visitLabel(getRatioStruct);
//		mv.visitLineNumber(121, getRatioStruct);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/RatioStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)V", false);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("numerator", "Ljava/math/BigInteger;", null, getDenominator, localVariables, 1);
		mv.visitLocalVariable("denominator", "Ljava/math/BigInteger;", null, getRatioStruct, localVariables, 2);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(4, 2);
		mv.visitEnd();
	}
}
