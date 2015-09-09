package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class MultipleValueProg1CodeGenerator implements CodeGenerator<MultipleValueProg1Struct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private static final String MULTIPLE_VALUE_PROG1_METHOD_NAME_PREFIX = "multipleValueProg1_";

	private static final String MULTIPLE_VALUE_PROG1_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	@Override
	public void generate(final MultipleValueProg1Struct input, final GeneratorState generatorState) {

		final LispStruct firstForm = input.getFirstForm();
		final PrognStruct forms = input.getForms();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String multipleValueProg1MethodName = MULTIPLE_VALUE_PROG1_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, multipleValueProg1MethodName, MULTIPLE_VALUE_PROG1_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		codeGenerator.generate(firstForm, generatorState);

		final int firstFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, firstFormStore);

		prognCodeGenerator.generate(forms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, firstFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, multipleValueProg1MethodName, MULTIPLE_VALUE_PROG1_METHOD_DESC, false);
	}
}
