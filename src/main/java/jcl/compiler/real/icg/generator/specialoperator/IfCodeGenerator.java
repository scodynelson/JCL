package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfCodeGenerator implements CodeGenerator<IfStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final IfStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final LispStruct testForm = input.getTestForm();
		formGenerator.generate(testForm, classBuilder);
		final int testFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		final Label elseLabel = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/lists/NullStruct", "INSTANCE", "Ljcl/lists/NullStruct;");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseLabel);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseLabel);

		final LispStruct thenForm = input.getThenForm();
		formGenerator.generate(thenForm, classBuilder);
		final int thenFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, thenFormStore);

		final Label ifEndLabel = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, ifEndLabel);

		mv.visitLabel(elseLabel);
		final LispStruct elseForm = input.getElseForm();
		formGenerator.generate(elseForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, thenFormStore);

		mv.visitLabel(ifEndLabel);

		mv.visitVarInsn(Opcodes.ALOAD, thenFormStore);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);
	}
}
