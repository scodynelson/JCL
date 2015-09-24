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
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
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

	private static final String FUNCTION_STRUCT_INIT_CLOSURE_DESC = "(Ljcl/functions/Closure;)V";
//	private static final String FUNCTION_STRUCT_INIT_CLOSURE_DESC = CodeGenerators.getConstructorDescription(FunctionStruct.class, Closure.class);

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME = "initLambdaListBindings";

	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC = "()V";
//	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME);

	private static final String GET_REQUIRED_BINDINGS_METHOD_NAME = "getRequiredBindings";

	private static final String GET_REQUIRED_BINDINGS_METHOD_DESC = "()Ljava/util/List;";
//	private static final String GET_REQUIRED_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_REQUIRED_BINDINGS_METHOD_NAME);

	private static final String GET_REQUIRED_BINDINGS_METHOD_SIGNATURE = "()Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/RequiredBinding;>;";

	private static final String GET_OPTIONAL_BINDINGS_METHOD_NAME = "getOptionalBindings";

	private static final String GET_OPTIONAL_BINDINGS_METHOD_DESC = "()Ljava/util/List;";
//	private static final String GET_OPTIONAL_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_OPTIONAL_BINDINGS_METHOD_NAME);

	private static final String GET_OPTIONAL_BINDINGS_METHOD_SIGNATURE = "()Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/OptionalBinding;>;";

	private static final String GET_REST_BINDING_METHOD_NAME = "getRestBinding";

	private static final String GET_REST_BINDING_METHOD_DESC = "()Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;";
//	private static final String GET_REST_BINDING_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_REST_BINDING_METHOD_NAME);

	private static final String GET_KEY_BINDINGS_METHOD_NAME = "getKeyBindings";

	private static final String GET_KEY_BINDINGS_METHOD_DESC = "()Ljava/util/List;";
//	private static final String GET_KEY_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_KEY_BINDINGS_METHOD_NAME);

	private static final String GET_KEY_BINDINGS_METHOD_SIGNATURE = "()Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/KeyBinding;>;";

	private static final String GET_ALLOW_OTHER_KEYS_METHOD_NAME = "getAllowOtherKeys";

	private static final String GET_ALLOW_OTHER_KEYS_METHOD_DESC = "()Z";
//	private static final String GET_ALLOW_OTHER_KEYS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_ALLOW_OTHER_KEYS_METHOD_NAME);

	private static final String GET_AUX_BINDINGS_METHOD_NAME = "getAuxBindings";

	private static final String GET_AUX_BINDINGS_METHOD_DESC = "()Ljava/util/List;";
//	private static final String GET_AUX_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_AUX_BINDINGS_METHOD_NAME);

	private static final String GET_AUX_BINDINGS_METHOD_SIGNATURE = "()Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/AuxBinding;>;";

	private static final String INTERNAL_APPLY_METHOD_NAME = "internalApply";

	private static final String INTERNAL_APPLY_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";
//	private static final String INTERNAL_APPLY_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, INTERNAL_APPLY_METHOD_NAME, Closure.class);

	private static final String GET_INIT_FORM_METHOD_NAME = "getInitForm";

	private static final String GET_INIT_FORM_METHOD_DESC = "(Ljcl/functions/Closure;Ljcl/symbols/SymbolStruct;)Ljcl/LispStruct;";
//	private static final String GET_INIT_FORM_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, GET_INIT_FORM_METHOD_NAME, Closure.class, SymbolStruct.class);

	private static final String GET_INIT_FORM_METHOD_SIGNATURE = "(Ljcl/functions/Closure;Ljcl/symbols/SymbolStruct<*>;)Ljcl/LispStruct;";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME = "initLoadTimeValueForms";

	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC = "(Ljcl/functions/Closure;)V";
//	private static final String INIT_LOAD_TIME_VALUE_FORMS_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, INIT_LOAD_TIME_VALUE_FORMS_METHOD_NAME, Closure.class);

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

		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();
		generateRequiredBindings(generatorState, lambdaListBindings, cw);
		generateOptionalBindings(generatorState, lambdaListBindings, cw);
		generateRestBinding(generatorState, lambdaListBindings, cw);
		generateKeyBindings(generatorState, lambdaListBindings, cw);
		generateAllowOtherKeys(generatorState, lambdaListBindings, cw);
		generateAuxBindings(generatorState, lambdaListBindings, cw);

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

	/**
	 * Private method for generating the class level {@link Component} annotation on the generated lambda class object
	 * being written to via the provided {@link ClassWriter}.
	 *
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the annotation code for
	 */
	private static void generateComponentAnnotation(final ClassWriter cw) {
		final AnnotationVisitor av = cw.visitAnnotation(COMPONENT_ANNOTATION_DESC, true);
		av.visitEnd();
	}

	/**
	 * Private method for generating the {@code serialVersionUID} field for the generated lambda class object being
	 * written to via the provided {@link ClassWriter}.
	 *
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the field code for
	 */
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

	/**
	 * Private method for generating the load-time-value field values contained in the {@link
	 * LambdaEnvironment#loadTimeValues} on the provided {@link LambdaStruct} input for the generated lambda class
	 * object being written to via the provided {@link ClassWriter}.
	 *
	 * @param input
	 * 		the {@link LambdaStruct} containing the {@link LambdaEnvironment#loadTimeValues} to generate fields for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the field code for
	 */
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

	/**
	 * Private method for generating the no-argument constructor for the generated lambda class object being written to
	 * via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the {@link Closure} argument constructor, passing 'null' as the {@link Closure}
	 * value</li>
	 * </ol>
	 * The following is the example Java code generated:
	 * <pre>
	 * {@code
	 * public Lambda_1() {
	 *      this((Closure) null);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param fileName
	 * 		the {@link String} containing the name of the current lambda class file name
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the field code for
	 */
	private static void generateNoArgConstructor(final GeneratorState generatorState, final String fileName,
	                                             final ClassWriter cw) {
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

	/**
	 * Private method for generating the {@link Closure} argument constructor for the generated lambda class object
	 * being written to via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the {@link FunctionStruct#FunctionStruct(String, Closure)} argument constructor via
	 * 'super', passing generated {@link LambdaStruct#docString} as a {@link String} constant and the provided {@link
	 * Closure} parameter value</li>
	 * <li>Generating the call to {@link FunctionStruct#initLoadTimeValueForms(Closure)}</li>
	 * <li>Generating the call to {@link FunctionStruct#initLambdaListBindings()}</li>
	 * </ol>
	 * The following is the example Java code generated:
	 * <pre>
	 * {@code
	 * public Lambda_1(Closure var1) {
	 *      super("Example Documentation String", var1);
	 *      this.initLoadTimeValueForms(var1);
	 *      this.initLambdaListBindings();
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link LambdaStruct} containing the {@link LambdaStruct#docString} to use for the documentation {@link
	 * 		String}
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param fileName
	 * 		the {@link String} containing the name of the current lambda class file name
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the field code for
	 */
	private static void generateClosureArgConstructor(final LambdaStruct input, final GeneratorState generatorState,
	                                                  final String fileName, final ClassWriter cw) {
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

	/**
	 * Private method for generating the {@link FunctionStruct#initLoadTimeValueForms(Closure)} method for the
	 * generated lambda class object being written to via the provided {@link ClassWriter}. The generation will perform
	 * the following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the list of {@link
	 * LambdaEnvironment#loadTimeValues} is empty</li>
	 * <li>Iterating through each of the {@link LambdaEnvironment#loadTimeValues} performing the following operations:
	 * <ol>
	 * <li>Generating the {@link Map.Entry#getValue()} as the value of the field</li>
	 * <li>Generating the code to set the field with the name of {@link Map.Entry#getKey()} to the previously generated
	 * value result</li>
	 * </ol>
	 * </li>
	 * </ol>
	 * The following is the example Java code generated:
	 * <pre>
	 * {@code
	 * protected void initLoadTimeValueForms(Closure var1) {
	 *      LispStruct var2 = this.symbolFunctionCall_1(var1);
	 *      var2 = ValuesStructs.extractPrimaryValue(var2);
	 *      this.loadTimeValue_1 = var2;
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link LambdaStruct} containing the {@link LambdaEnvironment#loadTimeValues} to generate the code to set
	 * 		their initial values
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param fileName
	 * 		the {@link String} containing the name of the current lambda class file name
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the field code for
	 */
	private void generateInitLoadTimeValueFormsMethod(final LambdaStruct input, final GeneratorState generatorState,
	                                                  final String fileName, final ClassWriter cw) {
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
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
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

	private void generateInternalApplyMethod(final LambdaStruct input, final GeneratorState generatorState,
	                                         final ClassWriter cw) {
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
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
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

	private void generateGetInitFormMethod(final LambdaStruct input, final GeneratorState generatorState,
	                                       final ClassWriter cw) {
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
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int closureStore = methodBuilder.getNextAvailableStore();
		final int symbolArgStore = methodBuilder.getNextAvailableStore();

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

	private static void generateRequiredBindings(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                             final ClassWriter cw) {
		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		if (requiredBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_REQUIRED_BINDINGS_METHOD_NAME,
				GET_REQUIRED_BINDINGS_METHOD_DESC,
				GET_REQUIRED_BINDINGS_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int requiredBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, requiredBindingsStore);

		final int requiredPackageStore = methodBuilder.getNextAvailableStore();
		final int requiredSymbolStore = methodBuilder.getNextAvailableStore();
		final int requiredBindingStore = methodBuilder.getNextAvailableStore();

		for (final RequiredBinding requiredBinding : requiredBindings) {
			final SymbolStruct<?> requiredSymbol = requiredBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(requiredSymbol, methodBuilder, requiredPackageStore, requiredSymbolStore);

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

		mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateOptionalBindings(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                      final ClassWriter cw) {
		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		if (optionalBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_OPTIONAL_BINDINGS_METHOD_NAME,
				GET_OPTIONAL_BINDINGS_METHOD_DESC,
				GET_OPTIONAL_BINDINGS_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int optionalBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, optionalBindingsStore);

		final int optionalPackageStore = methodBuilder.getNextAvailableStore();
		final int optionalSymbolStore = methodBuilder.getNextAvailableStore();
		final int optionalInitFormStore = methodBuilder.getNextAvailableStore();
		final int optionalSuppliedPSymbolStore = methodBuilder.getNextAvailableStore();
		final int optionalSuppliedPStore = methodBuilder.getNextAvailableStore();
		final int optionalBindingStore = methodBuilder.getNextAvailableStore();

		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(optionalSymbol, methodBuilder, optionalPackageStore, optionalSymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);

			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			generateSuppliedPBinding(suppliedPBinding, methodBuilder, optionalPackageStore, optionalSuppliedPSymbolStore, optionalSuppliedPStore);

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

		mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateRestBinding(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                        final ClassWriter cw) {
		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		if (restBinding == null) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_REST_BINDING_METHOD_NAME,
				GET_REST_BINDING_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		final int restPackageStore = methodBuilder.getNextAvailableStore();
		final int restSymbolStore = methodBuilder.getNextAvailableStore();

		final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
		CodeGenerators.generateSymbol(restSymbol, methodBuilder, restPackageStore, restSymbolStore);

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
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateKeyBindings(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                 final ClassWriter cw) {
		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		if (keyBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_KEY_BINDINGS_METHOD_NAME,
				GET_KEY_BINDINGS_METHOD_DESC,
				GET_KEY_BINDINGS_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int keyBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, keyBindingsStore);

		final int keyPackageStore = methodBuilder.getNextAvailableStore();
		final int keySymbolStore = methodBuilder.getNextAvailableStore();
		final int keyInitFormStore = methodBuilder.getNextAvailableStore();
		final int keyNameStore = methodBuilder.getNextAvailableStore();
		final int keySuppliedPSymbolStore = methodBuilder.getNextAvailableStore();
		final int keySuppliedPStore = methodBuilder.getNextAvailableStore();
		final int keyBindingStore = methodBuilder.getNextAvailableStore();

		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(keySymbol, methodBuilder, keyPackageStore, keySymbolStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);

			final SymbolStruct<?> keyName = keyBinding.getKeyName();
			CodeGenerators.generateSymbol(keyName, methodBuilder, keyPackageStore, keyNameStore);

			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			generateSuppliedPBinding(suppliedPBinding, methodBuilder, keyPackageStore, keySuppliedPSymbolStore, keySuppliedPStore);

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

		mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private static void generateSuppliedPBinding(final SuppliedPBinding suppliedPBinding, final JavaMethodBuilder methodBuilder,
	                                             final int packageStore, final int suppliedPSymbolStore, final int suppliedPStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (suppliedPBinding == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, suppliedPStore);
		} else {
			final SymbolStruct<?> keySuppliedPSymbol = suppliedPBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(keySuppliedPSymbol, methodBuilder, packageStore, suppliedPSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SUPPLIED_P_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, suppliedPSymbolStore);
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
			mv.visitVarInsn(Opcodes.ASTORE, suppliedPStore);
		}
	}

	private static void generateAllowOtherKeys(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                           final ClassWriter cw) {
		final boolean notAllowOtherKeys = !lambdaListBindings.isAllowOtherKeys();
		if (notAllowOtherKeys) {
			// No need to generate this method, as false is the default
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_ALLOW_OTHER_KEYS_METHOD_NAME,
				GET_ALLOW_OTHER_KEYS_METHOD_DESC,
				null,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

		mv.visitInsn(Opcodes.ICONST_1);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	private void generateAuxBindings(final GeneratorState generatorState, final OrdinaryLambdaListBindings lambdaListBindings,
	                                 final ClassWriter cw) {
		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
		if (auxBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
				GET_AUX_BINDINGS_METHOD_NAME,
				GET_AUX_BINDINGS_METHOD_DESC,
				GET_AUX_BINDINGS_METHOD_SIGNATURE,
				null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int auxBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, auxBindingsStore);

		final int auxPackageStore = methodBuilder.getNextAvailableStore();
		final int auxSymbolStore = methodBuilder.getNextAvailableStore();
		final int auxInitFormStore = methodBuilder.getNextAvailableStore();
		final int auxBindingStore = methodBuilder.getNextAvailableStore();

		for (final AuxBinding auxBinding : auxBindings) {
			final SymbolStruct<?> auxSymbol = auxBinding.getSymbolStruct();
			CodeGenerators.generateSymbol(auxSymbol, methodBuilder, auxPackageStore, auxSymbolStore);

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

		mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}
}
