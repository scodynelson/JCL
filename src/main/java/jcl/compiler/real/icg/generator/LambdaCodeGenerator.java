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
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
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

	private static final String COMPONENT_ANNOTATION_DESC = Type.getDescriptor(Component.class);

	private static final String LAMBDA_LIST_BINDINGS_FIELD = "lambdaListBindings";

	private static final String FUNCTION_STRUCT_INIT_CLOSURE_DESC = "(Ljcl/functions/Closure;)V";

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME = "initLambdaListBindings";

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC = "()V";

	private static final String INTERNAL_APPLY_METHOD_NAME = "internalApply";

	private static final String INTERNAL_APPLY_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_NAME = "getInitForm";

	private static final String GET_INIT_FORM_METHOD_DESC = "(Ljcl/functions/Closure;Ljcl/symbols/SymbolStruct;)Ljcl/LispStruct;";

	private static final String GET_INIT_FORM_METHOD_SIGNATURE = "(Ljcl/functions/Closure;Ljcl/symbols/SymbolStruct<*>;)Ljcl/LispStruct;";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME = "initLoadTimeValueForms";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC = "(Ljcl/functions/Closure;)V";

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

		generateComponentAnnotation(cw);
		generateSerialVersionUIDField(cw);
		generateLoadTimeValueFields(input, cw);
		generateNoArgConstructor(generatorState, fileName, cw);
		generateClosureArgConstructor(input, generatorState, fileName, cw);
		generateInitLoadTimeValueFormsMethod(input, generatorState, fileName, cw);
		generateInitLambdaListBindingsMethod(input, generatorState, fileName, cw);
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

	private static void generateComponentAnnotation(final ClassWriter cw) {
		final AnnotationVisitor av = cw.visitAnnotation(COMPONENT_ANNOTATION_DESC, true);
		av.visitEnd();
	}

	private static void generateSerialVersionUIDField(final ClassWriter cw) {
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
		final LambdaEnvironment environment = input.getLambdaEnvironment();

		final Map<String, LispStruct> loadTimeValues = environment.getLoadTimeValues();
		if (loadTimeValues.isEmpty()) {
			// No need to generate this method, as there are no load-time-value forms to initialize
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
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

		for (final Map.Entry<String, LispStruct> loadTimeValue : loadTimeValues.entrySet()) {
			final String uniqueLTVId = loadTimeValue.getKey();
			final LispStruct value = loadTimeValue.getValue();

			codeGenerator.generate(value, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, loadTimeValueInitFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, loadTimeValueInitFormStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
					GenerationConstants.VALUES_STRUCTS_NAME,
					GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_NAME,
					GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, loadTimeValueInitFormStore);

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

	private void generateInternalApplyMethod(final LambdaStruct input, final GeneratorState generatorState, final ClassWriter cw) {
		final PrognStruct forms = input.getForms();
		if (forms.getForms().isEmpty()) {
			// No need to generate this method, as there are no forms to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
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
		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
		if (optionalBindings.isEmpty() && keyBindings.isEmpty() && auxBindings.isEmpty()) {
			// No need to generate this method, as there are no init-forms to initialize
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_INIT_FORM_METHOD_NAME,
				GET_INIT_FORM_METHOD_DESC,
				GET_INIT_FORM_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureStore = methodBuilder.getNextAvailableStore();
		final int symbolArgStore = methodBuilder.getNextAvailableStore();

		// NOTE: commented out to allow proper scoping of dynamic variables. I think we just don't worry about the binding stack here.
//			bindingStack.push(lambdaEnvironment);

		final int initFormVarPackageStore = methodBuilder.getNextAvailableStore();
		final int initFormVarSymbolStore = methodBuilder.getNextAvailableStore();

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
