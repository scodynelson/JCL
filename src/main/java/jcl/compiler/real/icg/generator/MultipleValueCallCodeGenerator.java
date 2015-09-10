package jcl.compiler.real.icg.generator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class MultipleValueCallCodeGenerator extends SpecialOperatorCodeGenerator<MultipleValueCallStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	private static final String NOT_FUNCTION_ERROR_STRING = "MULTIPLE-VALUE-CALL: Invalid function form: ";

	private MultipleValueCallCodeGenerator() {
		super("multipleValueCall");
	}

	@Override
	protected void generateSpecialOperator(final MultipleValueCallStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final CompilerFunctionStruct functionForm = input.getFunctionForm();
		final List<LispStruct> forms = input.getForms();

		codeGenerator.generate(functionForm, generatorState);
		final int functionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionFormStore);

		final Label functionCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.FUNCTION_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, functionCheckIfEnd);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(NOT_FUNCTION_ERROR_STRING);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC,
				false);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(functionCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_STRUCT_NAME);
		final int realFunctionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realFunctionFormStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
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

			codeGenerator.generate(form, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, formStore);

			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
			mv.visitJumpInsn(Opcodes.IFEQ, elseStart);

			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.VALUES_STRUCT_NAME,
					GenerationConstants.VALUES_STRUCT_GET_VALUES_LIST_METHOD_NAME,
					GenerationConstants.VALUES_STRUCT_GET_VALUES_LIST_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, valuesListStore);

			mv.visitVarInsn(Opcodes.ALOAD, valuesListStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
					true);
			mv.visitVarInsn(Opcodes.ASTORE, iteratorStore);

			mv.visitLabel(iteratorLoopStart);

			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, iteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
			mv.visitVarInsn(Opcodes.ASTORE, valueStore);

			mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
			mv.visitVarInsn(Opcodes.ALOAD, valueStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);

			mv.visitJumpInsn(Opcodes.GOTO, iteratorLoopStart);

			mv.visitLabel(iteratorLoopEnd);
			mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

			mv.visitLabel(elseStart);
			mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(elseEnd);
		}

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int argsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_TO_ARRAY_METHOD_NAME,
				GenerationConstants.JAVA_LIST_TO_ARRAY_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_ARRAY_DESC);
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, realFunctionFormStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
