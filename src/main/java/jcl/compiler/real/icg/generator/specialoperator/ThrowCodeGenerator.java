package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
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

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "throwGen", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label getCatchTagValue = new Label();
		mv.visitLabel(getCatchTagValue);
//		mv.visitLineNumber(78, getCatchTagValue);
		final LispStruct catchTag = input.getCatchTag();
		formGenerator.generate(catchTag, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label getResultFormValue = new Label();
		mv.visitLabel(getResultFormValue);
//		mv.visitLineNumber(79, getResultFormValue);
		final LispStruct resultForm = input.getResultForm();
		formGenerator.generate(resultForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label throwThrowException = new Label();
		mv.visitLabel(throwThrowException);
//		mv.visitLineNumber(81, throwThrowException);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitVarInsn(Opcodes.ALOAD, 2);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("getCatchTagValue", "Ljcl/LispStruct;", null, getResultFormValue, localVariables, 1);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, throwThrowException, localVariables, 2);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(4, 2);
		mv.visitEnd();
	}
}
