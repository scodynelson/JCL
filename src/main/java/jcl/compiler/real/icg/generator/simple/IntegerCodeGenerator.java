package jcl.compiler.real.icg.generator.simple;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	@Override
	public void generate(final IntegerStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.getBigInteger();
		final String integerString = bigInteger.toString();
		mv.visitLdcInsn(integerString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		final int bigIntegerStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigIntegerStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/IntegerStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigIntegerStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/IntegerStruct", "<init>", "(Ljava/math/BigInteger;)V", false);
	}
}
