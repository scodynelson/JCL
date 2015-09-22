/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.security.SecureRandom;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Random;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class LambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	private static final String LAMBDA_LIST_BINDINGS_FIELD = "lambdaListBindings";

	private static final String INIT_FORM_PLACEHOLDER_FIELD = "INIT_FORM_PLACEHOLDER";

	private static final String CLOSURE_FIELD = "closure";

	private static final String FUNCTION_STRUCT_INIT_CLOSURE_DESC = "(Ljcl/functions/Closure;)V";

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME = "initLambdaListBindings";

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC = "()V";

	private static final String INTERNAL_APPLY_METHOD_NAME = "internalApply";

	private static final String INTERNAL_APPLY_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_NAME = "getInitForm";

	private static final String GET_INIT_FORM_METHOD_DESC = "(Ljcl/symbols/SymbolStruct;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_SIGNATURE = "(Ljcl/symbols/SymbolStruct<*>;)Ljcl/LispStruct;";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME = "initLoadTimeValueForms";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC = "(Ljcl/functions/Closure;)V";

	private static final String NON_LISP_ERROR_FOUND = "Non-Lisp error found.";

	@Override
	public void generate(final LambdaStruct input, final GeneratorState generatorState) {


		String fileName = input.getFileName();
		fileName = fileName.replace('.', '/');

		final String className = fileName.substring(fileName.lastIndexOf('/') + 1, fileName.length());

		final JavaClassBuilder currentClass = new JavaClassBuilder(fileName, className);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, fileName, null, GenerationConstants.FUNCTION_STRUCT_NAME, null);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		generateSerialVersionUIDField(cw);
		generateLoadTimeValueFields(input, cw);
		generateNoArgConstructor(generatorState, fileName, cw);
		generateClosureArgConstructor(input, generatorState, fileName, cw);
		generateInitLoadTimeValueFormsMethod(input, generatorState, fileName, cw);
		generateInitLambdaListBindingsMethod(input, generatorState, fileName, cw);
		generateApplyMethod(generatorState, fileName, cw);
		generateInternalApplyMethod(input, generatorState, cw);
		generateGetInitFormMethod(input, generatorState, cw);
//		generateClassInitMethod(generatorState, cw);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
		if (!classBuilderDeque.isEmpty()) {
			final JavaMethodBuilder previousMethodBuilder = generatorState.getCurrentMethodBuilder();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			previousMv.visitTypeInsn(Opcodes.NEW, fileName);
			previousMv.visitInsn(Opcodes.DUP);

			previousMv.visitVarInsn(Opcodes.ALOAD, 1); // Load the Closure Argument. NOTE: This should ALWAYS be 1 on the Store Stack
			previousMv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					fileName,
					GenerationConstants.INIT_METHOD_NAME,
					FUNCTION_STRUCT_INIT_CLOSURE_DESC,
					false);
		}
	}

	private void generateSerialVersionUIDField(final ClassWriter cw) {
		final Random random = new SecureRandom();
		final long serialVersionUID = random.nextLong();

		final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
				GenerationConstants.SERIAL_VERSION_UID_FIELD,
				GenerationConstants.JAVA_LONG_TYPE_NAME,
				null,
				serialVersionUID);

		fv.visitEnd();
	}

	private static void generateLoadTimeValueFields(final LambdaStruct input, final ClassWriter cw) {
		final LambdaEnvironment environment = input.getLambdaEnvironment();
		final Map<String, LispStruct> loadTimeValues = environment.getLoadTimeValues();
		for (final String uniqueLTVId : loadTimeValues.keySet()) {
			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL,
					uniqueLTVId,
					GenerationConstants.LISP_STRUCT_DESC,
					null,
					null);
			fv.visitEnd();
		}
	}

	private static void generateNoArgConstructor(final GeneratorState generatorState, final String fileName, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_INIT_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				fileName,
				GenerationConstants.INIT_METHOD_NAME,
				FUNCTION_STRUCT_INIT_CLOSURE_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateClosureArgConstructor(final LambdaStruct input, final GeneratorState generatorState, final String fileName, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				GenerationConstants.INIT_METHOD_NAME,
				FUNCTION_STRUCT_INIT_CLOSURE_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);

		String documentation = "";
		final StringStruct docString = input.getDocString();
		if (docString != null) {
			documentation = docString.getAsJavaString();
		}
		mv.visitLdcInsn(documentation);
		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_INIT_STRING_CLOSURE_DESC,
				false);

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				fileName,
				INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME,
				INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC,
				false);

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				fileName,
				INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME,
				INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC,
				false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateInitLoadTimeValueFormsMethod(final LambdaStruct input, final GeneratorState generatorState, final String fileName, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
				INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME,
				INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final int loadTimeValueInitFormStore = methodBuilder.getNextAvailableStore();

		final LambdaEnvironment environment = input.getLambdaEnvironment();
		final Map<String, LispStruct> loadTimeValues = environment.getLoadTimeValues();

		for (final Map.Entry<String, LispStruct> loadTimeValue : loadTimeValues.entrySet()) {
			final String uniqueLTVId = loadTimeValue.getKey();
			final LispStruct value = loadTimeValue.getValue();

			codeGenerator.generate(value, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, loadTimeValueInitFormStore);

			CodeGenerators.generateValuesCheckAndStore(methodBuilder, loadTimeValueInitFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitVarInsn(Opcodes.ALOAD, loadTimeValueInitFormStore);
			mv.visitFieldInsn(Opcodes.PUTFIELD, fileName, uniqueLTVId, GenerationConstants.LISP_STRUCT_DESC);
		}

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateInitLambdaListBindingsMethod(final LambdaStruct input, final GeneratorState generatorState, final String fileName, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
				INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME,
				INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		final int packageStore = methodBuilder.getNextAvailableStore();

		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();

		// End: Required
		final int requiredBindingsStore = methodBuilder.getNextAvailableStore();
		generateRequiredBindings(methodBuilder, lambdaListBindings, mv, packageStore, requiredBindingsStore);
		// End: Required

		// Start: Optional
		final int optionalBindingsStore = methodBuilder.getNextAvailableStore();
		generateOptionalBindings(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, optionalBindingsStore);
		// End: Optional

		// Start: Rest
		final int restBindingStore = methodBuilder.getNextAvailableStore();
		generateRestBinding(methodBuilder, lambdaListBindings, mv, packageStore, restBindingStore);
		// End: Rest

		// Start: Key
		final int keyBindingsStore = methodBuilder.getNextAvailableStore();
		generateKeyBindings(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, keyBindingsStore);
		// End: Key

		// Start: Allow-Other-Keys
		final int allowOtherKeysStore = methodBuilder.getNextAvailableStore();
		generateAllowOtherKeys(lambdaListBindings, mv, allowOtherKeysStore);
		// End: Allow-Other-Keys

		// Start: Aux
		final int auxBindingsStore = methodBuilder.getNextAvailableStore();
		generateAuxBindings(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, auxBindingsStore);
		// Start: End

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ORDINARY_LAMBDA_LIST_BINDINGS_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
		mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
		mv.visitVarInsn(Opcodes.ALOAD, restBindingStore);
		mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
		mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
		mv.visitVarInsn(Opcodes.ILOAD, allowOtherKeysStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.ORDINARY_LAMBDA_LIST_BINDINGS_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.ORDINARY_LAMBDA_LIST_BINDINGS_INIT_DESC,
				false);

		mv.visitFieldInsn(Opcodes.PUTFIELD, fileName, LAMBDA_LIST_BINDINGS_FIELD, GenerationConstants.ORDINARY_LAMBDA_LIST_BINDINGS_DESC);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateApplyMethod(final GeneratorState generatorState, final String fileName, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int argsStore = methodBuilder.getNextAvailableStore();

		final int closureSymbolBindingsStore = generateApplyClosureSymbolBindings(fileName, mv, methodBuilder, thisStore);
		final int closureFunctionBindingsStore = generateApplyClosureFunctionBindings(fileName, mv, methodBuilder, thisStore);
		final int parameterBindingsStore = generateApplyParameterBindings(fileName, mv, methodBuilder, thisStore, argsStore);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchErrorExceptionStart = new Label();
		final Label catchThrowableStart = new Label();
		final Label finallyBlockStart = new Label();
		final Label finallyBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchErrorExceptionStart, GenerationConstants.ERROR_EXCEPTION_NAME);
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchThrowableStart, GenerationConstants.JAVA_THROWABLE_NAME);
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
		mv.visitTryCatchBlock(catchErrorExceptionStart, finallyBlockStart, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETFIELD, fileName, CLOSURE_FIELD, GenerationConstants.CLOSURE_DESC);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				fileName,
				INTERNAL_APPLY_METHOD_NAME,
				INTERNAL_APPLY_METHOD_DESC,
				false);

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateApplyParameterUnbindings(mv, methodBuilder, parameterBindingsStore);
		generateApplyClosureFunctionUnbindings(mv, methodBuilder, closureFunctionBindingsStore);
		generateApplyClosureSymbolUnbindings(mv, methodBuilder, closureSymbolBindingsStore);
		mv.visitJumpInsn(Opcodes.GOTO, finallyBlockEnd);

		// Start: Catch ErrorException
		mv.visitLabel(catchErrorExceptionStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);
		// End: Catch ErrorException

		// Start: Catch Throwable
		mv.visitLabel(catchThrowableStart);
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitLdcInsn(NON_LISP_ERROR_FOUND);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.ERROR_EXCEPTION_INIT_STRING_THROWABLE_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
		// End: Catch Throwable

		mv.visitLabel(catchBlockStart);

		final int finallyExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, finallyExceptionStore);

		mv.visitLabel(finallyBlockStart);
		generateApplyParameterUnbindings(mv, methodBuilder, parameterBindingsStore);
		generateApplyClosureFunctionUnbindings(mv, methodBuilder, closureFunctionBindingsStore);
		generateApplyClosureSymbolUnbindings(mv, methodBuilder, closureSymbolBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, finallyExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(finallyBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static int generateApplyParameterBindings(final String fileName, final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int thisStore, final int argsStore) {
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				fileName,
				GenerationConstants.FUNCTION_STRUCT_GET_FUNCTION_BINDINGS_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_GET_FUNCTION_BINDINGS_METHOD_DESC,
				false);
		final int functionBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int parameterBindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterBindingIteratorStore);

		final Label parameterBindingIteratorLoopStart = new Label();
		final Label parameterBindingIteratorLoopEnd = new Label();

		mv.visitLabel(parameterBindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, parameterBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, parameterBindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, parameterBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME);
		final int functionParameterBindingStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionParameterBindingStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionParameterBindingStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_DESC,
				false);
		final int parameterSymbolToBindStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterSymbolToBindStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionParameterBindingStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_VALUE_METHOD_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_VALUE_METHOD_DESC,
				false);
		final int parameterValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterValueStore);

		CodeGenerators.generateValuesCheckAndStore(methodBuilder, parameterValueStore);

		final Label parameterInitFormCheckIfEnd = new Label();

		mv.visitFieldInsn(Opcodes.GETSTATIC, fileName, INIT_FORM_PLACEHOLDER_FIELD, GenerationConstants.LISP_STRUCT_DESC);
		mv.visitVarInsn(Opcodes.ALOAD, parameterValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.IFEQ, parameterInitFormCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToBindStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				fileName,
				GET_INIT_FORM_METHOD_NAME,
				GET_INIT_FORM_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, parameterValueStore);

		mv.visitLabel(parameterInitFormCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, functionParameterBindingStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_DESC,
				false);
		final int parameterIsSpecialStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, parameterIsSpecialStore);

		final Label parameterIsSpecialCheckElse = new Label();
		final Label parameterIsSpecialCheckElseEnd = new Label();

		mv.visitVarInsn(Opcodes.ILOAD, parameterIsSpecialStore);
		mv.visitJumpInsn(Opcodes.IFEQ, parameterIsSpecialCheckElse);

		mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToBindStore);
		mv.visitVarInsn(Opcodes.ALOAD, parameterValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.GOTO, parameterIsSpecialCheckElseEnd);

		mv.visitLabel(parameterIsSpecialCheckElse);

		mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToBindStore);
		mv.visitVarInsn(Opcodes.ALOAD, parameterValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC,
				false);

		mv.visitLabel(parameterIsSpecialCheckElseEnd);

		mv.visitJumpInsn(Opcodes.GOTO, parameterBindingIteratorLoopStart);

		mv.visitLabel(parameterBindingIteratorLoopEnd);
		return functionBindingsStore;
	}

	private static void generateApplyParameterUnbindings(final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int parameterBindingsStore) {
		mv.visitVarInsn(Opcodes.ALOAD, parameterBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int parameterUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterUnbindingIteratorStore);

		final Label parameterUnbindingIteratorLoopStart = new Label();
		final Label parameterUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(parameterUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, parameterUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, parameterUnbindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, parameterUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME);
		final int parameterUnbindingStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterUnbindingStore);

		mv.visitVarInsn(Opcodes.ALOAD, parameterUnbindingStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_DESC,
				false);
		final int parameterSymbolToUnbindStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, parameterSymbolToUnbindStore);

		mv.visitVarInsn(Opcodes.ALOAD, parameterUnbindingStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME,
				GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_DESC,
				false);
		final int parameterUnbindingIsSpecialStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, parameterUnbindingIsSpecialStore);

		final Label parameterUnbindingIsSpecialCheckElse = new Label();
		final Label parameterUnbindingIsSpecialCheckElseEnd = new Label();

		mv.visitVarInsn(Opcodes.ILOAD, parameterUnbindingIsSpecialStore);
		mv.visitJumpInsn(Opcodes.IFEQ, parameterUnbindingIsSpecialCheckElse);

		mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToUnbindStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.GOTO, parameterUnbindingIsSpecialCheckElseEnd);

		mv.visitLabel(parameterUnbindingIsSpecialCheckElse);

		mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToUnbindStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
				false);

		mv.visitLabel(parameterUnbindingIsSpecialCheckElseEnd);

		mv.visitJumpInsn(Opcodes.GOTO, parameterUnbindingIteratorLoopStart);

		mv.visitLabel(parameterUnbindingIteratorLoopEnd);
	}

	private static int generateApplyClosureSymbolBindings(final String fileName, final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int thisStore) {
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				fileName,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_SYMBOL_BINDINGS_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_SYMBOL_BINDINGS_METHOD_DESC,
				false);
		final int closureSymbolBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_SET_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_SET_METHOD_DESC,
				true);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_SET_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_DESC,
				true);
		final int closureSymbolBindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingIteratorStore);

		final Label closureSymbolBindingIteratorLoopStart = new Label();
		final Label closureSymbolBindingIteratorLoopEnd = new Label();

		mv.visitLabel(closureSymbolBindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, closureSymbolBindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.JAVA_MAP_ENTRY_NAME);
		final int closureSymbolBindingMapEntryStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingMapEntryStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingMapEntryStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_ENTRY_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_KEY_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_KEY_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int closureSymbolToBindStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolToBindStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingMapEntryStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_ENTRY_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_VALUE_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_VALUE_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		final int closureSymbolLexicalValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolLexicalValueStore);

		CodeGenerators.generateValuesCheckAndStore(methodBuilder, closureSymbolLexicalValueStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolToBindStore);
		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.GOTO, closureSymbolBindingIteratorLoopStart);

		mv.visitLabel(closureSymbolBindingIteratorLoopEnd);
		return closureSymbolBindingsStore;
	}

	private static void generateApplyClosureSymbolUnbindings(final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int closureSymbolBindingsStore) {
		mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_NAME,
				GenerationConstants.JAVA_MAP_KEY_SET_METHOD_NAME,
				GenerationConstants.JAVA_MAP_KEY_SET_METHOD_DESC,
				true);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_SET_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_DESC,
				true);
		final int closureUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureUnbindingIteratorStore);

		final Label closureSymbolUnbindingIteratorLoopStart = new Label();
		final Label closureSymbolUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(closureSymbolUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, closureUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, closureSymbolUnbindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, closureUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int closureUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureUnbindingMapKeySymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureUnbindingMapKeySymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.GOTO, closureSymbolUnbindingIteratorLoopStart);

		mv.visitLabel(closureSymbolUnbindingIteratorLoopEnd);
	}

	private static int generateApplyClosureFunctionBindings(final String fileName, final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int thisStore) {
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				fileName,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_FUNCTION_BINDINGS_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_FUNCTION_BINDINGS_METHOD_DESC,
				false);
		final int closureFunctionBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_SET_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_SET_METHOD_DESC,
				true);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_SET_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_DESC,
				true);
		final int closureFunctionBindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingIteratorStore);

		final Label closureFunctionBindingIteratorLoopStart = new Label();
		final Label closureFunctionBindingIteratorLoopEnd = new Label();

		mv.visitLabel(closureFunctionBindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, closureFunctionBindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.JAVA_MAP_ENTRY_NAME);
		final int closureFunctionBindingMapEntryStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingMapEntryStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingMapEntryStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_ENTRY_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_KEY_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_KEY_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int closureFunctionToBindStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionToBindStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingMapEntryStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_ENTRY_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_VALUE_METHOD_NAME,
				GenerationConstants.JAVA_MAP_ENTRY_GET_VALUE_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_STRUCT_NAME);
		final int closureFunctionValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionValueStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionToBindStore);
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_DESC,
				false);
		mv.visitJumpInsn(Opcodes.GOTO, closureFunctionBindingIteratorLoopStart);

		mv.visitLabel(closureFunctionBindingIteratorLoopEnd);
		return closureFunctionBindingsStore;
	}

	private static void generateApplyClosureFunctionUnbindings(final MethodVisitor mv, final JavaMethodBuilder methodBuilder, final int closureFunctionBindingsStore) {
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_MAP_NAME,
				GenerationConstants.JAVA_MAP_KEY_SET_METHOD_NAME,
				GenerationConstants.JAVA_MAP_KEY_SET_METHOD_DESC,
				true);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_SET_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_SET_ITERATOR_METHOD_DESC,
				true);
		final int closureFunctionUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionUnbindingIteratorStore);

		final Label closureFunctionUnbindingIteratorLoopStart = new Label();
		final Label closureFunctionUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(closureFunctionUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);

		mv.visitJumpInsn(Opcodes.IFEQ, closureFunctionUnbindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int closureFunctionUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionUnbindingMapKeySymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureFunctionUnbindingMapKeySymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC,
				false);
		mv.visitInsn(Opcodes.POP);
		mv.visitJumpInsn(Opcodes.GOTO, closureFunctionUnbindingIteratorLoopStart);

		mv.visitLabel(closureFunctionUnbindingIteratorLoopEnd);
	}

	private void generateInternalApplyMethod(final LambdaStruct input, final GeneratorState generatorState, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
				INTERNAL_APPLY_METHOD_NAME,
				INTERNAL_APPLY_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final LambdaEnvironment environment = input.getLambdaEnvironment();
		final PrognStruct forms = input.getForms();

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(environment);
		prognCodeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateGetInitFormMethod(final LambdaStruct input, final GeneratorState generatorState, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
				GET_INIT_FORM_METHOD_NAME,
				GET_INIT_FORM_METHOD_DESC,
				GET_INIT_FORM_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int symbolArgStore = methodBuilder.getNextAvailableStore();

		// NOTE: commented out to allow proper scoping of dynamic variables. I think we just don't worry about the binding stack here.
//			bindingStack.push(lambdaEnvironment);

		final int initFormVarPackageStore = methodBuilder.getNextAvailableStore();
		final int initFormVarSymbolStore = methodBuilder.getNextAvailableStore();

		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SymbolStruct<?> var = optionalBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(var, methodBuilder, initFormVarPackageStore, initFormVarSymbolStore);

			final Label symbolCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, symbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.IFEQ, symbolCheckIfEnd);

			final LispStruct initForm = optionalBinding.getInitForm();
			codeGenerator.generate(initForm, generatorState);
			mv.visitInsn(Opcodes.ARETURN);

			mv.visitLabel(symbolCheckIfEnd);
		}

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> var = keyBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(var, methodBuilder, initFormVarPackageStore, initFormVarSymbolStore);

			final Label symbolCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, symbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.IFEQ, symbolCheckIfEnd);

			final LispStruct initForm = keyBinding.getInitForm();
			codeGenerator.generate(initForm, generatorState);
			mv.visitInsn(Opcodes.ARETURN);

			mv.visitLabel(symbolCheckIfEnd);
		}

		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
		for (final AuxBinding auxBinding : auxBindings) {
			final SymbolStruct<?> var = auxBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(var, methodBuilder, initFormVarPackageStore, initFormVarSymbolStore);

			final Label symbolCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, symbolArgStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_NAME,
					GenerationConstants.JAVA_EQUALS_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.IFEQ, symbolCheckIfEnd);

			final LispStruct initForm = auxBinding.getInitForm();
			codeGenerator.generate(initForm, generatorState);
			mv.visitInsn(Opcodes.ARETURN);

			mv.visitLabel(symbolCheckIfEnd);
		}

//			bindingStack.pop();

		nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateClassInitMethod(final GeneratorState generatorState, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
				GenerationConstants.CLASS_INIT_METHOD_NAME,
				GenerationConstants.CLASS_INIT_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateRequiredBindings(final JavaMethodBuilder methodBuilder,
	                                             final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                             final int packageStore, final int requiredBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, requiredBindingsStore);

		final int requiredSymbolStore = methodBuilder.getNextAvailableStore();
		final int requiredBindingStore = methodBuilder.getNextAvailableStore();

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding requiredBinding : requiredBindings) {
			final SymbolStruct<?> requiredSymbol = requiredBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(requiredSymbol, methodBuilder, packageStore, requiredSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.REQUIRED_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredSymbolStore);
			if (requiredBinding.isSpecial()) {
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				mv.visitInsn(Opcodes.ICONST_0);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.REQUIRED_BINDING_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.REQUIRED_BINDING_INIT_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, requiredBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}
	}

	private void generateOptionalBindings(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                      final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                      final int packageStore, final int optionalBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, optionalBindingsStore);

		final int optionalSymbolStore = methodBuilder.getNextAvailableStore();
		final int optionalInitFormStore = methodBuilder.getNextAvailableStore();
		final int optionalSuppliedPSymbolStore = methodBuilder.getNextAvailableStore();
		final int optionalSuppliedPStore = methodBuilder.getNextAvailableStore();
		final int optionalBindingStore = methodBuilder.getNextAvailableStore();

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(optionalSymbol, methodBuilder, packageStore, optionalSymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
			} else {
				final SymbolStruct<?> optionalSuppliedPSymbol = suppliedPBinding.getSymbolStruct();
				CodeGenerators.generateSymbol(optionalSuppliedPSymbol, methodBuilder, packageStore, optionalSuppliedPSymbolStore);

				mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SUPPLIED_P_BINDING_NAME);
				mv.visitInsn(Opcodes.DUP);
				mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPSymbolStore);
				if (suppliedPBinding.isSpecial()) {
					mv.visitInsn(Opcodes.ICONST_1);
				} else {
					mv.visitInsn(Opcodes.ICONST_0);
				}
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
						GenerationConstants.SUPPLIED_P_BINDING_NAME,
						GenerationConstants.INIT_METHOD_NAME,
						GenerationConstants.SUPPLIED_P_BINDING_INIT_DESC,
						false);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
			}
			// End: Supplied-P

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.OPTIONAL_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, optionalSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalInitFormStore);
			if (optionalBinding.isSpecial()) {
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				mv.visitInsn(Opcodes.ICONST_0);
			}
			mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.OPTIONAL_BINDING_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.OPTIONAL_BINDING_INIT_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, optionalBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}
	}

	private static void generateRestBinding(final JavaMethodBuilder methodBuilder,
	                                        final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                        final int packageStore, final int restBindingStore) {

		final int restSymbolStore = methodBuilder.getNextAvailableStore();

		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		if (restBinding == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
		} else {
			final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(restSymbol, methodBuilder, packageStore, restSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.REST_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, restSymbolStore);
			if (restBinding.isSpecial()) {
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				mv.visitInsn(Opcodes.ICONST_0);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.REST_BINDING_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.REST_BINDING_INIT_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
		}
	}

	private void generateKeyBindings(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                 final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                 final int packageStore, final int keyBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, keyBindingsStore);

		final int keySymbolStore = methodBuilder.getNextAvailableStore();
		final int keyInitFormStore = methodBuilder.getNextAvailableStore();
		final int keyNameStore = methodBuilder.getNextAvailableStore();
		final int keySuppliedPSymbolStore = methodBuilder.getNextAvailableStore();
		final int keySuppliedPStore = methodBuilder.getNextAvailableStore();
		final int keyBindingStore = methodBuilder.getNextAvailableStore();

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(keySymbol, methodBuilder, packageStore, keySymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);

			final SymbolStruct<?> keyName = keyBinding.getKeyName();
			CodeGenerators.generateSymbol(keyName, methodBuilder, packageStore, keyNameStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
			} else {
				final SymbolStruct<?> keySuppliedPSymbol = suppliedPBinding.getSymbolStruct();
				CodeGenerators.generateSymbol(keySuppliedPSymbol, methodBuilder, packageStore, keySuppliedPSymbolStore);

				mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SUPPLIED_P_BINDING_NAME);
				mv.visitInsn(Opcodes.DUP);
				mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPSymbolStore);
				if (suppliedPBinding.isSpecial()) {
					mv.visitInsn(Opcodes.ICONST_1);
				} else {
					mv.visitInsn(Opcodes.ICONST_0);
				}
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
						GenerationConstants.SUPPLIED_P_BINDING_NAME,
						GenerationConstants.INIT_METHOD_NAME,
						GenerationConstants.SUPPLIED_P_BINDING_INIT_DESC,
						false);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
			}
			// End: Supplied-P

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.KEY_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, keySymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyInitFormStore);
			if (keyBinding.isSpecial()) {
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				mv.visitInsn(Opcodes.ICONST_0);
			}
			mv.visitVarInsn(Opcodes.ALOAD, keyNameStore);
			mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.KEY_BINDING_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.KEY_BINDING_INIT_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, keyBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}
	}

	private static void generateAllowOtherKeys(final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                           final int allowOtherKeysStore) {

		final boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();
		if (allowOtherKeys) {
			mv.visitInsn(Opcodes.ICONST_1);
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
		} else {
			mv.visitInsn(Opcodes.ICONST_0);
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
		}
	}

	private void generateAuxBindings(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                 final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                 final int packageStore, final int auxBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, auxBindingsStore);

		final int auxSymbolStore = methodBuilder.getNextAvailableStore();
		final int auxInitFormStore = methodBuilder.getNextAvailableStore();
		final int auxBindingStore = methodBuilder.getNextAvailableStore();

		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
		for (final AuxBinding auxBinding : auxBindings) {
			final SymbolStruct<?> auxSymbol = auxBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(auxSymbol, methodBuilder, packageStore, auxSymbolStore);

			// NOTE: Just generate a null value for this initForm here. We take care of the &aux initForms in the body
			//       when it is processed
			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, auxInitFormStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.AUX_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, auxSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxInitFormStore);
			if (auxBinding.isSpecial()) {
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				mv.visitInsn(Opcodes.ICONST_0);
			}
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.AUX_BINDING_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.AUX_BINDING_INIT_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, auxBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}
	}
}
