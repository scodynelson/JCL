package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.lists.NullStruct;
import jcl.symbols.NILStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class IfCodeGenerator extends SpecialOperatorCodeGenerator<IfStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	private IfCodeGenerator() {
		super("if_");
	}

	@Override
	public void generate(final IfStruct input, final GeneratorState generatorState) {

		final LispStruct testForm = input.getTestForm();
		final LispStruct thenForm = input.getThenForm();
		final LispStruct elseForm = input.getElseForm();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String ifMethodName = methodNamePrefix + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, ifMethodName, SPECIAL_OPERATOR_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		codeGenerator.generate(testForm, generatorState);
		final int testFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
		final int valuesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.VALUES_STRUCT_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		mv.visitLabel(valuesCheckIfEnd);

		final Label elseStart = new Label();
		final Label elseEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		nilCodeGenerator.generate(NILStruct.INSTANCE, generatorState);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		final int resultFormStore = methodBuilder.getNextAvailableStore();

		codeGenerator.generate(thenForm, generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

		mv.visitLabel(elseStart);
		codeGenerator.generate(elseForm, generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(elseEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, ifMethodName, SPECIAL_OPERATOR_METHOD_DESC, false);
	}
}
