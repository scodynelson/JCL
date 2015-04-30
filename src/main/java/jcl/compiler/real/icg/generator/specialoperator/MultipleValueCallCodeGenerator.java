package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallCodeGenerator implements CodeGenerator<MultipleValueCallStruct> {

	private static final String NOT_FUNCTION_ERROR_STRING = "MULTIPLE-VALUE-CALL: Invalid function form: ";

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final MultipleValueCallStruct input, final JavaClassBuilder classBuilder) {

		final CompilerFunctionStruct functionForm = input.getFunctionForm();
		final List<LispStruct> forms = input.getForms();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String multipleValueCallMethodName = "multipleValueCall_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, multipleValueCallMethodName, "(Ljcl/functions/Closure;)Ljcl/LispStruct;", null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		formGenerator.generate(functionForm, classBuilder);
		final int functionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionFormStore);

		final Label functionCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/functions/FunctionStruct");
		mv.visitJumpInsn(Opcodes.IFNE, functionCheckIfEnd);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/conditions/exceptions/ProgramErrorException");
		mv.visitInsn(Opcodes.DUP);

		mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
		mv.visitLdcInsn(NOT_FUNCTION_ERROR_STRING);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/conditions/exceptions/ProgramErrorException", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(functionCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/functions/FunctionStruct");
		final int realFunctionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realFunctionFormStore);

		mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
		final int argsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argsListStore);

		final int formStore = methodBuilder.getNextAvailableStore();
		final int valuesListStore = methodBuilder.getNextAvailableStore();
		final int iteratorStore = methodBuilder.getNextAvailableStore();
		final int valueStore = methodBuilder.getNextAvailableStore();

		for (final LispStruct form : forms) {
			final Label elseStart = new Label();
			final Label elseEnd = new Label();
			final Label iteratorLoopStart = new Label();
			final Label iteratorLoopEnd = new Label();

			formGenerator.generate(form, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, formStore);

			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
			mv.visitJumpInsn(Opcodes.IFEQ, elseStart);

			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getValuesList", "()Ljava/util/List;", false);
			mv.visitVarInsn(Opcodes.ASTORE, valuesListStore);

			mv.visitVarInsn(Opcodes.ALOAD, valuesListStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;", true);
			mv.visitVarInsn(Opcodes.ASTORE, iteratorStore);

			mv.visitLabel(iteratorLoopStart);

			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, iteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
			mv.visitVarInsn(Opcodes.ASTORE, valueStore);

			mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
			mv.visitVarInsn(Opcodes.ALOAD, valueStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);

			mv.visitJumpInsn(Opcodes.GOTO, iteratorLoopStart);

			mv.visitLabel(iteratorLoopEnd);
			mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

			mv.visitLabel(elseStart);
			mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(elseEnd);
		}

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "size", "()I", true);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "jcl/LispStruct");
		final int argsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "toArray", "([Ljava/lang/Object;)[Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "[Ljcl/LispStruct;");
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, realFunctionFormStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", false);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, multipleValueCallMethodName, "(Ljcl/functions/Closure;)Ljcl/LispStruct;", false);
	}
}
