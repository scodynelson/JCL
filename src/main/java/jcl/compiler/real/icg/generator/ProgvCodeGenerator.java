package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
final class ProgvCodeGenerator extends SpecialOperatorCodeGenerator<ProgvStruct> {

	private static final String SYMBOLS_LIST_MUST_BE_A_LIST = "PROGV: Symbols list must be a list. Got: ";

	private static final String ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS = "PROGV: Elements in symbols list must be symbols. Got: ";

	private static final String VALUES_LIST_MUST_BE_A_LIST = "PROGV: Values list must be a list. Got: ";

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private ProgvCodeGenerator() {
		super("progv");
	}

	@Override
	protected void generateSpecialOperator(final ProgvStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		// Generate and Check Vars List
		final LispStruct vars = input.getVars();
		final int varsJavaListStore = generateListAndCheck(vars, generatorState, methodBuilder, SYMBOLS_LIST_MUST_BE_A_LIST);
		generateVarListSymbolsCheck(methodBuilder, varsJavaListStore);

		// Generate and Check Vals List
		final LispStruct vals = input.getVals();
		final int valsJavaListStore = generateListAndCheck(vals, generatorState, methodBuilder, VALUES_LIST_MUST_BE_A_LIST);

		// Generate Dynamic Var Binding Loop
		generateVarBindingLoop(methodBuilder, varsJavaListStore, valsJavaListStore);

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		final ProgvEnvironment environment = input.getProgvEnvironment();
		final PrognStruct forms = input.getForms();

		final Stack<Environment> bindingStack = generatorState.getBindingStack();

		bindingStack.push(environment);
		prognCodeGenerator.generate(forms, generatorState);
		bindingStack.pop();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(methodBuilder, varsJavaListStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(methodBuilder, varsJavaListStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	private int generateListAndCheck(final LispStruct possibleList, final GeneratorState generatorState,
	                                 final JavaMethodBuilder methodBuilder, final String mustBeListErrorString) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(possibleList, generatorState);

		final int inputStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputStore);

		final Label inputListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, inputStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.LIST_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, inputListCheckEnd);

		generateProgramError(mv, inputStore, mustBeListErrorString);

		mv.visitLabel(inputListCheckEnd);

		mv.visitVarInsn(Opcodes.ALOAD, inputStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LIST_STRUCT_NAME);
		final int inputListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputListStore);

		mv.visitVarInsn(Opcodes.ALOAD, inputListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.LIST_STRUCT_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC,
				false);
		final int inputJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputJavaListStore);

		return inputJavaListStore;
	}

	private static void generateVarListSymbolsCheck(final JavaMethodBuilder methodBuilder, final int varsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int varsJavaListIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsJavaListIteratorStore);

		final Label varsListOfSymbolsIteratorLoopStart = new Label();
		final Label varsListOfSymbolsIteratorLoopEnd = new Label();

		mv.visitLabel(varsListOfSymbolsIteratorLoopStart);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, varsListOfSymbolsIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		final int varSymbolCheckStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolCheckStore);

		final Label varSymbolCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolCheckStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.SYMBOL_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, varSymbolCheckEnd);

		generateProgramError(mv, varSymbolCheckStore, ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS);

		mv.visitLabel(varSymbolCheckEnd);

		mv.visitJumpInsn(Opcodes.GOTO, varsListOfSymbolsIteratorLoopStart);

		mv.visitLabel(varsListOfSymbolsIteratorLoopEnd);
	}

	private static void generateVarBindingLoop(final JavaMethodBuilder methodBuilder, final int varsJavaListStore,
	                                           final int valsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
				true);
		final int varsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, varsJavaListSizeStore);

		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
				true);
		final int valsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, valsJavaListSizeStore);

		mv.visitInsn(Opcodes.ICONST_0);
		final int indexValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, indexValueStore);

		final Label varBindingLoopStart = new Label();
		final Label varBindingLoopEnd = new Label();

		mv.visitLabel(varBindingLoopStart);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, varsJavaListSizeStore);
		mv.visitJumpInsn(Opcodes.IF_ICMPGE, varBindingLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int varSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		final int valStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, valsJavaListSizeStore);

		final Label numberOfValsCheckEnd = new Label();

		mv.visitJumpInsn(Opcodes.IF_ICMPGE, numberOfValsCheckEnd);
		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitLabel(numberOfValsCheckEnd);

		CodeGenerators.generateValuesCheckAndStore(methodBuilder, valStore);

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitIincInsn(indexValueStore, 1);
		mv.visitJumpInsn(Opcodes.GOTO, varBindingLoopStart);

		mv.visitLabel(varBindingLoopEnd);
	}

	private static void generateFinallyCode(final JavaMethodBuilder methodBuilder, final int varsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int unbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, unbindingIteratorStore);

		final Label unbindingIteratorLoopStart = new Label();
		final Label unbindingIteratorLoopEnd = new Label();

		mv.visitLabel(unbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, unbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, unbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, unbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int unbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, unbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, unbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitJumpInsn(Opcodes.GOTO, unbindingIteratorLoopStart);

		mv.visitLabel(unbindingIteratorLoopEnd);
	}

	private static void generateProgramError(final MethodVisitor mv, final int varsStore, final String errorString) {
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(errorString);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
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
	}
}
