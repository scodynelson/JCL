package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.simple.NILCodeGenerator;
import jcl.compiler.real.icg.generator.simple.NullCodeGenerator;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.lists.NullStruct;
import jcl.symbols.NILStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfCodeGenerator implements CodeGenerator<IfStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	@Override
	public void generate(final IfStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct testForm = input.getTestForm();
		final LispStruct thenForm = input.getThenForm();
		final LispStruct elseForm = input.getElseForm();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		formGenerator.generate(testForm, classBuilder);
		final int testFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
		final int valuesStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		mv.visitLabel(valuesCheckIfEnd);

		final Label elseStart = new Label();
		final Label elseEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		nilCodeGenerator.generate(NILStruct.INSTANCE, classBuilder);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		final int resultFormStore = currentClass.getNextAvailableStore();

		formGenerator.generate(thenForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

		mv.visitLabel(elseStart);
		formGenerator.generate(elseForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(elseEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);
	}
}
