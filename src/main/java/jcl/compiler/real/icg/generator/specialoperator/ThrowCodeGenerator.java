package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowCodeGenerator implements CodeGenerator<ThrowStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ThrowStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct catchTag = input.getCatchTag();
		final LispStruct resultForm = input.getResultForm();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();
		final MethodVisitor previousMv = currentClass.getMethodVisitor();

		final String throwMethodName = "throw_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, throwMethodName, "()Ljcl/LispStruct;", null, null);
		currentClass.setMethodVisitor(mv);
		mv.visitCode();

		formGenerator.generate(catchTag, classBuilder);
		final int catchTagStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		formGenerator.generate(resultForm, classBuilder);
		final int resultFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		currentClass.setMethodVisitor(previousMv);

		previousMv.visitVarInsn(Opcodes.ALOAD, 0);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, throwMethodName, "()Ljcl/LispStruct;", false);
	}
}
