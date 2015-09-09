/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.security.SecureRandom;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.GeneratorState;
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
public class LambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	public static final String FUNCTION_STRUCT_INIT_CLOSURE_DESC = "(Ljcl/functions/Closure;)V";

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	private static final String LAMBDA_LIST_BINDINGS_FIELD = "lambdaListBindings";

	private static final String INIT_FORM_PLACEHOLDER_FIELD = "INIT_FORM_PLACEHOLDER";

	private static final String CLOSURE_FIELD = "closure";

	private static final String INTERNAL_APPLY_METHOD_NAME = "internalApply";

	private static final String INTERNAL_APPLY_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_NAME = "getInitForm";

	private static final String GET_INIT_FORM_METHOD_DESC = "(Ljcl/symbols/SymbolStruct;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_SIGNATURE = "(Ljcl/symbols/SymbolStruct<*>;)Ljcl/LispStruct;";

	private static final String NON_LISP_ERROR_FOUND = "Non-Lisp error found.";

	@Override
	public void generate(final LambdaStruct input, final GeneratorState generatorState) {

		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();
		final StringStruct docString = input.getDocString();
		final PrognStruct forms = input.getForms();
		final LambdaEnvironment lambdaEnvironment = input.getLambdaEnvironment();

		String fileName = input.getFileName();
		fileName = fileName.replace('.', '/');

		final String className = fileName.substring(fileName.lastIndexOf('/') + 1, fileName.length());

		final JavaClassBuilder currentClass = new JavaClassBuilder(fileName, className);
		final Stack<JavaClassBuilder> classStack = generatorState.getClassStack();

		classStack.push(currentClass);
		generatorState.setCurrentClass(currentClass);
		generatorState.getClasses().addFirst(currentClass);

		final Stack<Environment> bindingStack = generatorState.getBindingStack();

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, fileName, null, GenerationConstants.FUNCTION_STRUCT_NAME, null);

		cw.visitSource(className + GenerationConstants.JAVA_EXTENSION, null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
					GenerationConstants.SERIAL_VERSION_UID_FIELD,
					GenerationConstants.JAVA_LONG_TYPE_NAME,
					null,
					serialVersionUID);

			fv.visitEnd();
		}
		{
			final List<LoadTimeValue> loadTimeValues = lambdaEnvironment.getLoadTimeValues();
			for (final LoadTimeValue loadTimeValue : loadTimeValues) {
				final String uniqueLTVId = loadTimeValue.getUniqueLTVId();

				final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
						uniqueLTVId,
						GenerationConstants.LISP_STRUCT_DESC,
						null,
						null);
				fv.visitEnd();
			}
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.FUNCTION_STRUCT_INIT_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

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

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
					GenerationConstants.INIT_METHOD_NAME,
					FUNCTION_STRUCT_INIT_CLOSURE_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int closureStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			String documentation = "";
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
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					fileName,
					"initLambdaListBindings",
					"()V",
					false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
					"initLambdaListBindings",
					"()V",
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			final int packageStore = methodBuilder.getNextAvailableStore();

			// End: Required
			final int requiredBindingsStore = methodBuilder.getNextAvailableStore();
			generateRequiredBindings(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, requiredBindingsStore);
			// End: Required

			// Start: Optional
			final int optionalBindingsStore = methodBuilder.getNextAvailableStore();
			generateOptionalBindings(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, optionalBindingsStore);
			// End: Optional

			// Start: Rest
			final int restBindingStore = methodBuilder.getNextAvailableStore();
			generateRestBinding(generatorState, methodBuilder, lambdaListBindings, mv, packageStore, restBindingStore);
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

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.O_LAMBDA_LIST_BINDINGS_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, restBindingStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
			mv.visitVarInsn(Opcodes.ILOAD, allowOtherKeysStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.O_LAMBDA_LIST_BINDINGS_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.O_LAMBDA_LIST_BINDINGS_INIT_DESC,
					false);

			mv.visitFieldInsn(Opcodes.PUTFIELD, fileName, LAMBDA_LIST_BINDINGS_FIELD, GenerationConstants.O_LAMBDA_LIST_BINDINGS_DESC);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS,
					GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
					GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int argsStore = methodBuilder.getNextAvailableStore();

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

			// START: Bind Closure Symbol values
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

			final Label closureSymbolValuesCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
			mv.visitJumpInsn(Opcodes.IFEQ, closureSymbolValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
			final int closureSymbolValuesStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolValuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolValuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.VALUES_STRUCT_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolLexicalValueStore);

			mv.visitLabel(closureSymbolValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolToBindStore);
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, closureSymbolBindingIteratorLoopStart);

			mv.visitLabel(closureSymbolBindingIteratorLoopEnd);
			// END: Bind Closure Symbol values

			// START: Bind Closure Function values
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
			// END: Bind Closure Function values

			// START: Bind Parameter values
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

			final Label parameterValuesCheckIfEnd = new Label();
			final Label parameterInitFormCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, parameterValueStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
			mv.visitJumpInsn(Opcodes.IFEQ, parameterValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, parameterValueStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
			final int parameterValuesStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterValuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, parameterValuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.VALUES_STRUCT_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, parameterValueStore);

			mv.visitLabel(parameterValuesCheckIfEnd);

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
			// END: Bind Parameter values

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

			// START: Unbind Parameter values
			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
					true);
			final int normalParameterUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalParameterUnbindingIteratorStore);

			final Label normalParameterUnbindingIteratorLoopStart = new Label();
			final Label normalParameterUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalParameterUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalParameterUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME);
			final int normalParameterUnbindingStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalParameterUnbindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_DESC,
					false);
			final int normalParameterSymbolToUnbindStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalParameterSymbolToUnbindStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_DESC,
					false);
			final int normalParameterUnbindingIsSpecialStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ISTORE, normalParameterUnbindingIsSpecialStore);

			final Label normalParameterUnbindingIsSpecialCheckElse = new Label();
			final Label normalParameterUnbindingIsSpecialCheckElseEnd = new Label();

			mv.visitVarInsn(Opcodes.ILOAD, normalParameterUnbindingIsSpecialStore);
			mv.visitJumpInsn(Opcodes.IFEQ, normalParameterUnbindingIsSpecialCheckElse);

			mv.visitVarInsn(Opcodes.ALOAD, normalParameterSymbolToUnbindStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, normalParameterUnbindingIsSpecialCheckElseEnd);

			mv.visitLabel(normalParameterUnbindingIsSpecialCheckElse);

			mv.visitVarInsn(Opcodes.ALOAD, normalParameterSymbolToUnbindStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);

			mv.visitLabel(normalParameterUnbindingIsSpecialCheckElseEnd);

			mv.visitJumpInsn(Opcodes.GOTO, normalParameterUnbindingIteratorLoopStart);

			mv.visitLabel(normalParameterUnbindingIteratorLoopEnd);
			// END: Unbind Parameter values

			// START: Unbind Closure Function values
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
			final int normalClosureFunctionUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureFunctionUnbindingIteratorStore);

			final Label normalClosureFunctionUnbindingIteratorLoopStart = new Label();
			final Label normalClosureFunctionUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalClosureFunctionUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalClosureFunctionUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
			final int normalClosureFunctionUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureFunctionUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, normalClosureFunctionUnbindingIteratorLoopStart);

			mv.visitLabel(normalClosureFunctionUnbindingIteratorLoopEnd);
			// END: Unbind Closure Function values

			// START: Unbind Closure Symbol values
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
			final int normalClosureUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureUnbindingIteratorStore);

			final Label normalClosureSymbolUnbindingIteratorLoopStart = new Label();
			final Label normalClosureSymbolUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalClosureSymbolUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalClosureSymbolUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
			final int normalClosureUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, normalClosureSymbolUnbindingIteratorLoopStart);

			mv.visitLabel(normalClosureSymbolUnbindingIteratorLoopEnd);
			// END: Unbind Closure Symbol values

			mv.visitJumpInsn(Opcodes.GOTO, finallyBlockEnd);

			final int exceptionStore = methodBuilder.getNextAvailableStore();

			mv.visitLabel(catchErrorExceptionStart);
			mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(catchThrowableStart);
			mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ERROR_EXCEPTION_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(NON_LISP_ERROR_FOUND);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, GenerationConstants.ERROR_EXCEPTION_NAME, GenerationConstants.INIT_METHOD_NAME, "(Ljava/lang/String;Ljava/lang/Throwable;)V", false);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(catchBlockStart);

			final int finallyExceptionStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, finallyExceptionStore);

			mv.visitLabel(finallyBlockStart);

			// START: Unbind Parameter values
			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
					true);
			final int exceptionParameterUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionParameterUnbindingIteratorStore);

			final Label exceptionParameterUnbindingIteratorLoopStart = new Label();
			final Label exceptionParameterUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionParameterUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionParameterUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME);
			final int exceptionParameterUnbindingStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionParameterUnbindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_DESC,
					false);
			final int exceptionParameterSymbolToUnbindStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionParameterSymbolToUnbindStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME,
					GenerationConstants.FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_DESC,
					false);
			final int exceptionParameterUnbindingIsSpecialStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ISTORE, exceptionParameterUnbindingIsSpecialStore);

			final Label exceptionParameterUnbindingIsSpecialCheckElse = new Label();
			final Label exceptionParameterUnbindingIsSpecialCheckElseEnd = new Label();

			mv.visitVarInsn(Opcodes.ILOAD, exceptionParameterUnbindingIsSpecialStore);
			mv.visitJumpInsn(Opcodes.IFEQ, exceptionParameterUnbindingIsSpecialCheckElse);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterSymbolToUnbindStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionParameterUnbindingIsSpecialCheckElseEnd);

			mv.visitLabel(exceptionParameterUnbindingIsSpecialCheckElse);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterSymbolToUnbindStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);

			mv.visitLabel(exceptionParameterUnbindingIsSpecialCheckElseEnd);

			mv.visitJumpInsn(Opcodes.GOTO, exceptionParameterUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionParameterUnbindingIteratorLoopEnd);
			// END: Unbind Parameter values

			// START: Unbind Closure Function values
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
			final int exceptionClosureFunctionUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureFunctionUnbindingIteratorStore);

			final Label exceptionClosureFunctionUnbindingIteratorLoopStart = new Label();
			final Label exceptionClosureFunctionUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionClosureFunctionUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionClosureFunctionUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
			final int exceptionClosureFunctionUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureFunctionUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionClosureFunctionUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionClosureFunctionUnbindingIteratorLoopEnd);
			// END: Unbind Closure Function values

			// START: Unbind Closure Symbol values
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
			final int exceptionClosureUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureUnbindingIteratorStore);

			final Label exceptionClosureUnbindingIteratorLoopStart = new Label();
			final Label exceptionClosureUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionClosureUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
					true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionClosureUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_ITERATOR_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
					GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
					true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
			final int exceptionClosureUnbindingMapKeySymbolStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionClosureUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionClosureUnbindingIteratorLoopEnd);
			// END: Unbind Closure Symbol values

			mv.visitVarInsn(Opcodes.ALOAD, finallyExceptionStore);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(finallyBlockEnd);
			mv.visitVarInsn(Opcodes.ALOAD, resultStore);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
					INTERNAL_APPLY_METHOD_NAME,
					INTERNAL_APPLY_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int closureArgStore = methodBuilder.getNextAvailableStore();

			bindingStack.push(lambdaEnvironment);
			prognCodeGenerator.generate(forms, generatorState);
			bindingStack.pop();

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
					GET_INIT_FORM_METHOD_NAME,
					GET_INIT_FORM_METHOD_DESC,
					GET_INIT_FORM_METHOD_SIGNATURE,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int symbolArgStore = methodBuilder.getNextAvailableStore();

			// NOTE: commented out to allow proper scoping of dynamic variables. I think we just don't worry about the binding stack here.
//			bindingStack.push(lambdaEnvironment);

			final int initFormVarPackageStore = methodBuilder.getNextAvailableStore();
			final int initFormVarSymbolStore = methodBuilder.getNextAvailableStore();

			final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
			for (final OptionalBinding optionalBinding : optionalBindings) {
				final SymbolStruct<?> var = optionalBinding.getSymbolStruct();
				SymbolCodeGeneratorUtil.generate(var, generatorState, initFormVarPackageStore, initFormVarSymbolStore);

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
				formGenerator.generate(initForm, generatorState);
				mv.visitInsn(Opcodes.ARETURN);

				mv.visitLabel(symbolCheckIfEnd);
			}

			final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
			for (final KeyBinding keyBinding : keyBindings) {
				final SymbolStruct<?> var = keyBinding.getSymbolStruct();
				SymbolCodeGeneratorUtil.generate(var, generatorState, initFormVarPackageStore, initFormVarSymbolStore);

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
				formGenerator.generate(initForm, generatorState);
				mv.visitInsn(Opcodes.ARETURN);

				mv.visitLabel(symbolCheckIfEnd);
			}

			final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
			for (final AuxBinding auxBinding : auxBindings) {
				final SymbolStruct<?> var = auxBinding.getSymbolStruct();
				SymbolCodeGeneratorUtil.generate(var, generatorState, initFormVarPackageStore, initFormVarSymbolStore);

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
				formGenerator.generate(initForm, generatorState);
				mv.visitInsn(Opcodes.ARETURN);

				mv.visitLabel(symbolCheckIfEnd);
			}

//			bindingStack.pop();

			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
					GenerationConstants.CLASS_INIT_METHOD_NAME,
					GenerationConstants.CLASS_INIT_METHOD_DESC,
					null,
					null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			methodBuilderStack.push(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			final int initFormStore = methodBuilder.getNextAvailableStore();

			final List<LoadTimeValue> loadTimeValues = lambdaEnvironment.getLoadTimeValues();
			for (final LoadTimeValue loadTimeValue : loadTimeValues) {
				final String uniqueLTVId = loadTimeValue.getUniqueLTVId();
				final LispStruct value = loadTimeValue.getValue();

				formGenerator.generate(value, generatorState);
				mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

				final Label valuesCheckIfEnd = new Label();

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
				mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
				final int valuesStore = methodBuilder.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

				mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.VALUES_STRUCT_NAME,
						GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
						GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
						false);
				mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

				mv.visitLabel(valuesCheckIfEnd);

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitFieldInsn(Opcodes.PUTSTATIC, fileName, uniqueLTVId, GenerationConstants.LISP_STRUCT_DESC);
			}

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderStack.pop();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final JavaClassBuilder previousJavaClassBuilder = classStack.peek();
			generatorState.setCurrentClass(previousJavaClassBuilder);

			final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
			final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
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

	private void generateRequiredBindings(final GeneratorState classBuilder, final JavaMethodBuilder methodBuilder,
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
			SymbolCodeGeneratorUtil.generate(requiredSymbol, classBuilder, packageStore, requiredSymbolStore);

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

	private void generateOptionalBindings(final GeneratorState classBuilder, final JavaMethodBuilder methodBuilder,
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
			SymbolCodeGeneratorUtil.generate(optionalSymbol, classBuilder, packageStore, optionalSymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
			} else {
				final SymbolStruct<?> optionalSuppliedPSymbol = suppliedPBinding.getSymbolStruct();
				SymbolCodeGeneratorUtil.generate(optionalSuppliedPSymbol, classBuilder, packageStore, optionalSuppliedPSymbolStore);

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

	private void generateRestBinding(final GeneratorState classBuilder, final JavaMethodBuilder methodBuilder,
	                                 final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                 final int packageStore, final int restBindingStore) {

		final int restSymbolStore = methodBuilder.getNextAvailableStore();

		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		if (restBinding == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
		} else {
			final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
			SymbolCodeGeneratorUtil.generate(restSymbol, classBuilder, packageStore, restSymbolStore);

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

	private void generateKeyBindings(final GeneratorState classBuilder, final JavaMethodBuilder methodBuilder,
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
			SymbolCodeGeneratorUtil.generate(keySymbol, classBuilder, packageStore, keySymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);

			final SymbolStruct<?> keyName = keyBinding.getKeyName();
			SymbolCodeGeneratorUtil.generate(keyName, classBuilder, packageStore, keyNameStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
			} else {
				final SymbolStruct<?> keySuppliedPSymbol = suppliedPBinding.getSymbolStruct();
				SymbolCodeGeneratorUtil.generate(keySuppliedPSymbol, classBuilder, packageStore, keySuppliedPSymbolStore);

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

	private void generateAllowOtherKeys(final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
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

	private void generateAuxBindings(final GeneratorState classBuilder, final JavaMethodBuilder methodBuilder,
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
			SymbolCodeGeneratorUtil.generate(auxSymbol, classBuilder, packageStore, auxSymbolStore);

			// NOTE: Just generate a null value for this initForm here. We take care of the &aux initForms in the body
			//       when it is processed
			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
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
