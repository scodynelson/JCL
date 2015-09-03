package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvCodeGenerator implements CodeGenerator<ProgvStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private static final String PROGV_METHOD_NAME_PREFIX = "progv_";

	private static final String PROGV_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String SYMBOLS_LIST_MUST_BE_A_LIST = "PROGV: Symbols list must be a list. Got: ";

	private static final String ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS = "PROGV: Elements in symbols list must be symbols. Got: ";

	private static final String VALUES_LIST_MUST_BE_A_LIST = "PROGV: Values list must be a list. Got: ";

	@Override
	public void generate(final ProgvStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct vars = input.getVars();
		final LispStruct vals = input.getVals();
		final PrognStruct forms = input.getForms();
		final ProgvEnvironment progvEnvironment = input.getProgvEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String progvMethodName = PROGV_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, progvMethodName, PROGV_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		final Label finallyBlockStart = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
		mv.visitTryCatchBlock(catchBlockStart, finallyBlockStart, catchBlockStart, null);

		formGenerator.generate(vars, classBuilder);

		final int varsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsStore);

		final Label varsListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.LIST_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, varsListCheckEnd);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(SYMBOLS_LIST_MUST_BE_A_LIST);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(varsListCheckEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LIST_STRUCT_NAME);
		final int varsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsListStore);

		mv.visitVarInsn(Opcodes.ALOAD, varsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.LIST_STRUCT_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC,
				false);
		final int varsJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsJavaListStore);

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

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, varSymbolCheckStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(varSymbolCheckEnd);

		mv.visitJumpInsn(Opcodes.GOTO, varsListOfSymbolsIteratorLoopStart);

		mv.visitLabel(varsListOfSymbolsIteratorLoopEnd);

		formGenerator.generate(vals, classBuilder);

		final int valsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsStore);

		final Label valsListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.LIST_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, valsListCheckEnd);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(VALUES_LIST_MUST_BE_A_LIST);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TOSTRING_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(valsListCheckEnd);
		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LIST_STRUCT_NAME);
		final int valsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsListStore);

		mv.visitVarInsn(Opcodes.ALOAD, valsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.LIST_STRUCT_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC,
				false);
		final int valsJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsJavaListStore);

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

		// Var Binding Loop

		mv.visitInsn(Opcodes.ICONST_0);
		final int indexValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, indexValueStore);

		final Label varBindingLoopStart = new Label();

		mv.visitLabel(varBindingLoopStart);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, varsJavaListSizeStore);
		mv.visitJumpInsn(Opcodes.IF_ICMPGE, tryBlockStart);

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

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
		final int valuesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.VALUES_STRUCT_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitLabel(valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitIincInsn(indexValueStore, 1);
		mv.visitJumpInsn(Opcodes.GOTO, varBindingLoopStart);

		// BODY START
		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(progvEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int normalUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingIteratorStore);

		final Label normalUnbindingIteratorLoopStart = new Label();
		final Label normalUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(normalUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, normalUnbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		final int normalUnbindingVarStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingVarStore);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingVarStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int normalUnbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitJumpInsn(Opcodes.GOTO, normalUnbindingIteratorLoopStart);

		mv.visitLabel(normalUnbindingIteratorLoopEnd);

		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		mv.visitLabel(finallyBlockStart);
		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int exceptionUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingIteratorStore);

		final Label exceptionUnbindingIteratorLoopStart = new Label();
		final Label exceptionUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(exceptionUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, exceptionUnbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		final int exceptionUnbindingVarStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingVarStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingVarStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int exceptionUnbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitJumpInsn(Opcodes.GOTO, exceptionUnbindingIteratorLoopStart);

		mv.visitLabel(exceptionUnbindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, progvMethodName, PROGV_METHOD_DESC, false);
	}
}
