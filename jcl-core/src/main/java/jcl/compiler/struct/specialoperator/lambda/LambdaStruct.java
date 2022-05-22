/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.lambda;

import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Class to generate {@link LambdaStruct} object dynamically by creating a new Java class with the name of the {@link
 * LambdaStruct#className} value, the supported parameter methods for the {@link LambdaStruct#lambdaListBindings}
 * values, and the execution body containing the {@link LambdaStruct#forms} values.
 */
@Getter
public class LambdaStruct extends CompilerSpecialOperatorStruct {

	private final String className;
	private final OrdinaryLambdaList lambdaListBindings;
	private final StringStruct docString;
	private final PrognStruct forms;
	private final Environment lambdaEnvironment;

	public LambdaStruct(final String className, final OrdinaryLambdaList lambdaListBindings, final StringStruct docString,
	                    final PrognStruct forms, final Environment lambdaEnvironment) {
		super("lambda");
		this.className = className;
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	@Override
	public String toString() {
		return "(LAMBDA " + lambdaListBindings + ' ' + docString + ' ' + forms + ')';
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing.
	}

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#initLambdaListBindings()}
	 * method.
	 */
	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME = "initLambdaListBindings";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#initLambdaListBindings()}
	 * method.
	 */
	private static final String INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)V";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getRequiredBindings()} method.
	 */
	private static final String GET_REQUIRED_BINDINGS_METHOD_NAME = "getRequiredBindings";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getRequiredBindings()}
	 * method.
	 */
	private static final String GET_REQUIRED_BINDINGS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljava/util/List;";

	/**
	 * Constant {@link String} containing the signature for the {@link CompiledFunctionStruct#getRequiredBindings()}
	 * method.
	 */
	private static final String GET_REQUIRED_BINDINGS_METHOD_SIGNATURE = "(Ljcl/compiler/environment/Environment;)Ljava/util/List<Ljcl/compiler/environment/binding/lambdalist/RequiredParameter;>;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getOptionalBindings()} method.
	 */
	private static final String GET_OPTIONAL_BINDINGS_METHOD_NAME = "getOptionalBindings";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getOptionalBindings()}
	 * method.
	 */
	private static final String GET_OPTIONAL_BINDINGS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljava/util/List;";

	/**
	 * Constant {@link String} containing the signature for the {@link CompiledFunctionStruct#getOptionalBindings()}
	 * method.
	 */
	private static final String GET_OPTIONAL_BINDINGS_METHOD_SIGNATURE = "(Ljcl/compiler/environment/Environment;)Ljava/util/List<Ljcl/compiler/environment/binding/lambdalist/OptionalParameter;>;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getRestBinding()} method.
	 */
	private static final String GET_REST_BINDING_METHOD_NAME = "getRestBinding";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getRestBinding()}
	 * method.
	 */
	private static final String GET_REST_BINDING_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljcl/compiler/environment/binding/lambdalist/RestParameter;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getKeyBindings()} method.
	 */
	private static final String GET_KEY_BINDINGS_METHOD_NAME = "getKeyBindings";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getKeyBindings()}
	 * method.
	 */
	private static final String GET_KEY_BINDINGS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljava/util/List;";

	/**
	 * Constant {@link String} containing the signature for the {@link CompiledFunctionStruct#getKeyBindings()} method.
	 */
	private static final String GET_KEY_BINDINGS_METHOD_SIGNATURE = "(Ljcl/compiler/environment/Environment;)Ljava/util/List<Ljcl/compiler/environment/binding/lambdalist/KeyParameter;>;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getAllowOtherKeys()} method.
	 */
	private static final String GET_ALLOW_OTHER_KEYS_METHOD_NAME = "getAllowOtherKeys";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getAllowOtherKeys()}
	 * method.
	 */
	private static final String GET_ALLOW_OTHER_KEYS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Z";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getAuxBindings()} method.
	 */
	private static final String GET_AUX_BINDINGS_METHOD_NAME = "getAuxBindings";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getAuxBindings()}
	 * method.
	 */
	private static final String GET_AUX_BINDINGS_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljava/util/List;";

	/**
	 * Constant {@link String} containing the signature for the {@link CompiledFunctionStruct#getAuxBindings()} method.
	 */
	private static final String GET_AUX_BINDINGS_METHOD_SIGNATURE = "(Ljcl/compiler/environment/Environment;)Ljava/util/List<Ljcl/compiler/environment/binding/lambdalist/AuxParameter;>;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#internalApply(Environment)}
	 * method.
	 */
	private static final String INTERNAL_APPLY_METHOD_NAME = "internalApply";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#internalApply(Environment)}
	 * method.
	 */
	private static final String INTERNAL_APPLY_METHOD_DESC = "(Ljcl/compiler/environment/Environment;)Ljcl/lang/LispStruct;";

	/**
	 * Constant {@link String} containing the name for the {@link CompiledFunctionStruct#getInitForm(Environment,
	 * SymbolStruct)} method.
	 */
	private static final String GET_INIT_FORM_METHOD_NAME = "getInitForm";

	/**
	 * Constant {@link String} containing the description for the {@link CompiledFunctionStruct#getInitForm(Environment,
	 * SymbolStruct)} method.
	 */
	private static final String GET_INIT_FORM_METHOD_DESC = "(Ljcl/compiler/environment/Environment;Ljcl/lang/SymbolStruct;)Ljcl/lang/LispStruct;";

	/**
	 * Constant {@link String} containing the signature for the {@link CompiledFunctionStruct#getInitForm(Environment,
	 * SymbolStruct)} method.
	 */
	private static final String GET_INIT_FORM_METHOD_SIGNATURE = "(Ljcl/compiler/environment/Environment;Ljcl/lang/SymbolStruct;)Ljcl/lang/LispStruct;";

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LambdaStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Generating the code for the load-time-value fields</li>
	 * <li>Generating the code for the no-argument constructor</li>
	 * <li>Generating the code for the {@link Environment} argument constructor</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getRequiredBindings()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getOptionalBindings()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getRestBinding()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getKeyBindings()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getAllowOtherKeys()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getAuxBindings()} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#internalApply(Environment)} method</li>
	 * <li>Generating the code for the {@link CompiledFunctionStruct#getInitForm(Environment, SymbolStruct)} method</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * <li>If the {@link GeneratorState#classBuilderDeque} is not empty after the visitation for this new class,
	 * performing the following operations:
	 * <ol>
	 * <li>Generating the code to instantiate the newly created class in the previous method execution stack</li>
	 * </ol>
	 * </li>
	 * </ol>
	 * As an example, it will transform {@code (lambda ())} into the following Java code:
	 * <pre>
	 * {@code
	 * package jcl;
	 *
	 * import jcl.compiler.environment.Environment;
	 * import jcl.compiler.function.CompiledFunctionStruct;
	 *
	 * public class Lambda_1 extends CompiledFunctionStruct {
	 *
	 *      public Lambda_1() {
	 *          this(Environment.NULL);
	 *      }
	 *
	 *      public Lambda_1(Environment var1) {
	 *          super("", var1);
	 *          this.initLambdaListBindings();
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {

		final String fileName = CodeGenerators.getFileNameFromClassName(className);

		final JavaClassBuilder currentClass = new JavaClassBuilder(className, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(GenerationConstants.JAVA_VERSION, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         className,
		         null,
		         GenerationConstants.COMPILED_FUNCTION_STRUCT_NAME,
		         null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateNoArgConstructor(generatorState, className, cw);
		generateEnvironmentArgConstructor(generatorState, className, cw);

		final Set<SymbolStruct> existingLexicalSymbols = new HashSet<>(generatorState.getLexicalSymbols());
		final Set<SymbolStruct> existingDynamicSymbols = new HashSet<>(generatorState.getDynamicSymbols());

		generateRequiredBindings(generatorState, lambdaListBindings, cw);
		generateOptionalBindings(generatorState, lambdaListBindings, cw);
		generateRestBinding(generatorState, lambdaListBindings, cw);
		generateKeyBindings(generatorState, lambdaListBindings, cw);
		generateAllowOtherKeys(generatorState, lambdaListBindings, cw);
		generateAuxBindings(generatorState, lambdaListBindings, cw);

		generateInternalApplyMethod(generatorState, cw);
		generateGetInitFormMethod(generatorState, cw);
//		generateClassInitMethod(generatorState, cw);

		removeGeneratorSymbolEntries(generatorState, existingLexicalSymbols, existingDynamicSymbols);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
		if (!classBuilderDeque.isEmpty()) {
			final JavaMethodBuilder previousMethodBuilder = generatorState.getCurrentMethodBuilder();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			previousMv.visitTypeInsn(Opcodes.NEW, className);
			previousMv.visitInsn(Opcodes.DUP);

			previousMv.visitVarInsn(Opcodes.ALOAD, 1); // Load the Environment Argument. NOTE: This should ALWAYS be 1 on the Store Stack
			previousMv.visitMethodInsn(Opcodes.INVOKESPECIAL,
			                           className,
			                           GenerationConstants.INIT_METHOD_NAME,
			                           GenerationConstants.COMPILED_FUNCTION_STRUCT_INIT_ENVIRONMENT_DESC,
			                           false);
		}
	}

	private void removeGeneratorSymbolEntries(final GeneratorState generatorState,
	                                          final Set<SymbolStruct> existingLexicalSymbols,
	                                          final Set<SymbolStruct> existingDynamicSymbols) {
		for (final RequiredParameter requiredBinding : lambdaListBindings.getRequiredBindings()) {
			final SymbolStruct var = requiredBinding.getVar();
			if (requiredBinding.isSpecial()) {
				if (!existingDynamicSymbols.contains(var)) {
					generatorState.getDynamicSymbols().remove(var);
				}
			} else {
				if (!existingLexicalSymbols.contains(var)) {
					generatorState.getLexicalSymbols().remove(var);
				}
			}
		}
		for (final OptionalParameter optionalBinding : lambdaListBindings.getOptionalBindings()) {
			final SymbolStruct var = optionalBinding.getVar();
			if (optionalBinding.isSpecial()) {
				if (!existingDynamicSymbols.contains(var)) {
					generatorState.getDynamicSymbols().remove(var);
				}
			} else {
				if (!existingLexicalSymbols.contains(var)) {
					generatorState.getLexicalSymbols().remove(var);
				}
			}
			final Optional<SuppliedPParameter> suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPVar = suppliedPBinding.get().getVar();
				if (suppliedPBinding.get().isSpecial()) {
					if (!existingDynamicSymbols.contains(suppliedPVar)) {
						generatorState.getDynamicSymbols().remove(var);
					}
				} else {
					if (!existingLexicalSymbols.contains(var)) {
						generatorState.getLexicalSymbols().remove(var);
					}
				}
			}
		}
		final RestParameter restBinding = lambdaListBindings.getRestBinding();
		if (restBinding != null) {
			final SymbolStruct var = restBinding.getVar();
			if (restBinding.isSpecial()) {
				if (!existingDynamicSymbols.contains(var)) {
					generatorState.getDynamicSymbols().remove(var);
				}
			} else {
				if (!existingLexicalSymbols.contains(var)) {
					generatorState.getLexicalSymbols().remove(var);
				}
			}
		}
		for (final KeyParameter keyBinding : lambdaListBindings.getKeyBindings()) {
			final SymbolStruct var = keyBinding.getVar();
			if (keyBinding.isSpecial()) {
				if (!existingDynamicSymbols.contains(var)) {
					generatorState.getDynamicSymbols().remove(var);
				}
			} else {
				if (!existingLexicalSymbols.contains(var)) {
					generatorState.getLexicalSymbols().remove(var);
				}
			}
			final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPVar = suppliedPBinding.get().getVar();
				if (suppliedPBinding.get().isSpecial()) {
					if (!existingDynamicSymbols.contains(suppliedPVar)) {
						generatorState.getDynamicSymbols().remove(var);
					}
				} else {
					if (!existingLexicalSymbols.contains(var)) {
						generatorState.getLexicalSymbols().remove(var);
					}
				}
			}
		}
		for (final AuxParameter auxBinding : lambdaListBindings.getAuxBindings()) {
			final SymbolStruct var = auxBinding.getVar();
			if (auxBinding.isSpecial()) {
				if (!existingDynamicSymbols.contains(var)) {
					generatorState.getDynamicSymbols().remove(var);
				}
			} else {
				if (!existingLexicalSymbols.contains(var)) {
					generatorState.getLexicalSymbols().remove(var);
				}
			}
		}
	}

	/**
	 * Private method for generating the no-argument constructor for the generated lambda class object being written to
	 * via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the {@link Environment} argument constructor, passing {@link Environment#NULL} as the
	 * {@link Environment} value</li>
	 * </ol>
	 * The following is the example Java code generated:
	 * <pre>
	 * {@code
	 * public Lambda_1() {
	 *      this(Environment.NULL);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param className
	 * 		the {@link String} containing the name of the current lambda class file name
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 */
	private static void generateNoArgConstructor(final GeneratorState generatorState, final String className,
	                                             final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.ENVIRONMENT_NAME,
		                  GenerationConstants.ENVIRONMENT_NULL_NAME,
		                  GenerationConstants.ENVIRONMENT_DESC);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   className,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.COMPILED_FUNCTION_STRUCT_INIT_ENVIRONMENT_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link Environment} argument constructor for the generated lambda class object
	 * being written to via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the {@link CompiledFunctionStruct#CompiledFunctionStruct(String, Environment)} argument
	 * constructor via 'super', passing generated {@link LambdaStruct#docString} as a {@link String} constant and the
	 * provided {@link Environment} parameter value</li>
	 * <li>Generating the call to {@link CompiledFunctionStruct#initLambdaListBindings()}</li>
	 * </ol>
	 * The following is the example Java code generated:
	 * <pre>
	 * {@code
	 * public Lambda_1(Environment var1) {
	 *      super("Example Documentation String", var1);
	 *      this.initLambdaListBindings();
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param className
	 * 		the {@link String} containing the name of the current lambda class file name
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 */
	private void generateEnvironmentArgConstructor(final GeneratorState generatorState,
	                                               final String className, final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.COMPILED_FUNCTION_STRUCT_INIT_ENVIRONMENT_DESC,
		                                        null,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getThisStore();
		final int environmentStore = methodBuilder.getEnvironmentStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);

		String documentation = "";
		if (docString != null) {
			documentation = docString.toJavaString();
		}
		mv.visitLdcInsn(documentation);
		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.COMPILED_FUNCTION_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.COMPILED_FUNCTION_STRUCT_INIT_STRING_ENVIRONMENT_DESC,
		                   false);

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   className,
		                   INIT_LAMBDA_LIST_BINDINGS_METHOD_NAME,
		                   INIT_LAMBDA_LIST_BINDINGS_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#internalApply(Environment)} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link LambdaStruct#forms} are
	 * empty</li>
	 * <li>Temporarily pushing the {@link LambdaStruct#lambdaEnvironment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link LambdaStruct#forms} values, ensuring
	 * to
	 * return the final value as the method result</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda () 1)} is encountered:
	 * <pre>
	 * {@code
	 * protected LispStruct internalApply(Environment var1) {
	 *      BigInteger var2 = new BigInteger("1");
	 *      return new IntegerStruct(var2);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private void generateInternalApplyMethod(final GeneratorState generatorState,
	                                         final ClassWriter cw) {
		if (forms.getForms().isEmpty()) {
			// No need to generate this method, as there are no forms to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        INTERNAL_APPLY_METHOD_NAME,
		                                        INTERNAL_APPLY_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		forms.generate(generatorState);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getInitForm(Environment, SymbolStruct)} method for
	 * the generated lambda class object being written to via the provided {@link ClassWriter}. The generation will
	 * perform the following operations:
	 * <ol>
	 * <li>Generating a condition check using {@link SymbolStruct#eq(LispStruct)} to determine if the provided {@link
	 * SymbolStruct} provided to the method is equivalent to one of the 'optional', 'key', or 'aux' function
	 * parameters</li>
	 * <li>Generating the resulting {@link LispStruct} value of the init-form for each of the 'optional', 'key', and
	 * 'aux' function parameters</li>
	 * <li>Generating the {@link NILStruct#INSTANCE} singleton to be used when none of the 'optional', 'key', or 'aux'
	 * function parameters match the provided {@link SymbolStruct}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&optional (y 2)) y)} is encountered:
	 * <pre>
	 * {@code
	 * protected LispStruct getInitForm(Environment var1, SymbolStruct var2) {
	 *      PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var4 = var3.intern("Y").getSymbol();
	 *      if(var2.eq(var4)) {
	 *          BigInteger var5 = new BigInteger("2");
	 *          return new IntegerStruct(var5);
	 *      } else {
	 *          return NILStruct.INSTANCE;
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private void generateGetInitFormMethod(final GeneratorState generatorState,
	                                       final ClassWriter cw) {
		final List<OptionalParameter> optionalBindings = lambdaListBindings.getOptionalBindings();
		final List<KeyParameter> keyBindings = lambdaListBindings.getKeyBindings();
		final List<AuxParameter> auxBindings = lambdaListBindings.getAuxBindings();
		if (optionalBindings.isEmpty() && keyBindings.isEmpty() && auxBindings.isEmpty()) {
			// No need to generate this method, as there are no init-forms to initialize
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_INIT_FORM_METHOD_NAME,
		                                        GET_INIT_FORM_METHOD_DESC,
		                                        GET_INIT_FORM_METHOD_SIGNATURE,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

		final int symbolArgStore = methodBuilder.getNextAvailableStore();

		final int initFormVarPackageStore = methodBuilder.getNextAvailableStore();
		final int initFormVarSymbolStore = methodBuilder.getNextAvailableStore();

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final SymbolStruct var = optionalBinding.getVar();
			final LispStruct initForm = optionalBinding.getInitForm();

			generateInitForm(generatorState, methodBuilder, symbolArgStore,
			                 initFormVarPackageStore, initFormVarSymbolStore, var, initForm);
		}

		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct var = keyBinding.getVar();
			final LispStruct initForm = keyBinding.getInitForm();

			generateInitForm(generatorState, methodBuilder, symbolArgStore,
			                 initFormVarPackageStore, initFormVarSymbolStore, var, initForm);
		}

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStruct var = auxBinding.getVar();
			final LispStruct initForm = auxBinding.getInitForm();

			generateInitForm(generatorState, methodBuilder, symbolArgStore,
			                 initFormVarPackageStore, initFormVarSymbolStore, var, initForm);
		}

		NILStruct.INSTANCE.generate(generatorState);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method used for assisting the generation of the {@link CompiledFunctionStruct#getInitForm(Environment,
	 * SymbolStruct)} method, generating the {@link SymbolStruct} equality check as well as the {@link LispStruct}
	 * value of the init-form.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building the Java method body for the {@link
	 * 		CompiledFunctionStruct#getInitForm(Environment, SymbolStruct)} method
	 * @param symbolArgStore
	 * 		the storage location index on the stack where the {@link SymbolStruct} parameter value is located
	 * @param initFormVarPackageStore
	 * 		the storage location index on the stack where the {@link PackageStruct} for the provided {@code var} {@link
	 * 		SymbolStruct} will exist
	 * @param initFormVarSymbolStore
	 * 		the storage location index on the stack where the provided {@code var} {@link SymbolStruct} will exist
	 * @param var
	 * 		the {@link SymbolStruct} variable to be used as the source of the equality check against the {@link
	 * 		SymbolStruct} value at the provided {@code symbolArgStore} storage location index on the stack
	 * @param initForm
	 * 		the {@link LispStruct} init-form value to be generated as the value to be used when the {@link SymbolStruct}
	 * 		matching equality to the provided {@code var} {@link SymbolStruct} is encountered
	 */
	private void generateInitForm(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                              final int symbolArgStore, final int initFormVarPackageStore,
	                              final int initFormVarSymbolStore, final SymbolStruct var, final LispStruct initForm) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		CodeGenerators.generateSymbol(var, generatorState, initFormVarPackageStore, initFormVarSymbolStore);

		final Label symbolCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, symbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.LISP_STRUCT_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_DESC,
		                   true);
		mv.visitJumpInsn(Opcodes.IFEQ, symbolCheckIfEnd);

		initForm.generate(generatorState);
		mv.visitInsn(Opcodes.ARETURN);

		mv.visitLabel(symbolCheckIfEnd);
	}

	/*
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
	*/

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getRequiredBindings()} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * OrdinaryLambdaList#requiredBindings} is empty</li>
	 * <li>Generating the {@link List} for containing the resulting {@link RequiredParameter}s</li>
	 * <li>Generating each of the {@link RequiredParameter}s and adding them to the previously created {@link
	 * List}</li>
	 * <li>Generating the code to return the generated {@link List} of {@link RequiredParameter}s</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (a) a)} is encountered:
	 * <pre>
	 * {@code
	 * protected List<RequiredBinding> getRequiredBindings() {
	 *      ArrayList var1 = new ArrayList();
	 *
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("A").getSymbol();
	 *      RequiredBinding var4 = new RequiredBinding(var3, false);
	 *      var1.add(var4);
	 *
	 *      return var1;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@link List} of {@link RequiredParameter}s to generate
	 * 		code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private static void generateRequiredBindings(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
	                                             final ClassWriter cw) {
		final List<RequiredParameter> requiredBindings = lambdaListBindings.getRequiredBindings();
		if (requiredBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_REQUIRED_BINDINGS_METHOD_NAME,
		                                        GET_REQUIRED_BINDINGS_METHOD_DESC,
		                                        GET_REQUIRED_BINDINGS_METHOD_SIGNATURE,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

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

		for (final RequiredParameter requiredBinding : requiredBindings) {
			final SymbolStruct requiredSymbol = requiredBinding.getVar();
			CodeGenerators.generateSymbol(requiredSymbol, generatorState, requiredPackageStore, requiredSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.REQUIRED_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredSymbolStore);
			if (requiredBinding.isSpecial()) {
				generatorState.getDynamicSymbols().add(requiredSymbol);
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				generatorState.getLexicalSymbols().add(requiredSymbol);
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

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getOptionalBindings()} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * OrdinaryLambdaList#optionalBindings} is empty</li>
	 * <li>Generating the {@link List} for containing the resulting {@link OptionalParameter}s</li>
	 * <li>Generating each of the {@link OptionalParameter}s and adding them to the previously created {@link
	 * List}</li>
	 * <li>Generating the code to return the generated {@link List} of {@link OptionalParameter}s</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&optional (b 2 b-p) b)} is encountered:
	 * <pre>
	 * {@code
	 * protected List<OptionalBinding> getOptionalBindings() {
	 *      ArrayList var1 = new ArrayList();
	 *
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("B").getSymbol();
	 *      NILStruct var4 = NILStruct.INSTANCE;
	 *
	 *      var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var5 = var2.intern("B-P").getSymbol();
	 *      SuppliedPBinding var6 = new SuppliedPBinding(var5, false);
	 *
	 *      OptionalBinding var7 = new OptionalBinding(var3, var4, false, var6);
	 *      var1.add(var7);
	 *
	 *      return var1;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@link List} of {@link OptionalParameter}s to generate
	 * 		code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private void generateOptionalBindings(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
	                                      final ClassWriter cw) {
		final List<OptionalParameter> optionalBindings = lambdaListBindings.getOptionalBindings();
		if (optionalBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_OPTIONAL_BINDINGS_METHOD_NAME,
		                                        GET_OPTIONAL_BINDINGS_METHOD_DESC,
		                                        GET_OPTIONAL_BINDINGS_METHOD_SIGNATURE,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

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

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final SymbolStruct optionalSymbol = optionalBinding.getVar();
			CodeGenerators.generateSymbol(optionalSymbol, generatorState, optionalPackageStore, optionalSymbolStore);

			NILStruct.INSTANCE.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);

			final Optional<SuppliedPParameter> suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				generateSuppliedPBinding(suppliedPBinding.get(), generatorState, optionalPackageStore, optionalSuppliedPSymbolStore, optionalSuppliedPStore);
			} else {
				generateSuppliedPBinding(null, generatorState, optionalPackageStore, optionalSuppliedPSymbolStore, optionalSuppliedPStore);
			}

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.OPTIONAL_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, optionalSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalInitFormStore);
			if (optionalBinding.isSpecial()) {
				generatorState.getDynamicSymbols().add(optionalSymbol);
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				generatorState.getLexicalSymbols().add(optionalSymbol);
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

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getRestBinding()} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link
	 * OrdinaryLambdaList#restBinding} is null</li>
	 * <li>Generating and returning the {@link RestParameter}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&rest x) x)} is encountered:
	 * <pre>
	 * {@code
	 * protected RestBinding getRestBinding() {
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("X").getSymbol();
	 *      return new RestBinding(var2, false);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@link RestParameter} to generate code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private static void generateRestBinding(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
	                                        final ClassWriter cw) {
		final RestParameter restBinding = lambdaListBindings.getRestBinding();
		if (restBinding == null) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_REST_BINDING_METHOD_NAME,
		                                        GET_REST_BINDING_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

		final int restPackageStore = methodBuilder.getNextAvailableStore();
		final int restSymbolStore = methodBuilder.getNextAvailableStore();

		final SymbolStruct restSymbol = restBinding.getVar();
		CodeGenerators.generateSymbol(restSymbol, generatorState, restPackageStore, restSymbolStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.REST_BINDING_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, restSymbolStore);
		if (restBinding.isSpecial()) {
			generatorState.getDynamicSymbols().add(restSymbol);
			mv.visitInsn(Opcodes.ICONST_1);
		} else {
			generatorState.getLexicalSymbols().add(restSymbol);
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

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getKeyBindings()} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * OrdinaryLambdaList#keyBindings} is empty</li>
	 * <li>Generating the {@link List} for containing the resulting {@link KeyParameter}s</li>
	 * <li>Generating each of the {@link KeyParameter}s and adding them to the previously created {@link List}</li>
	 * <li>Generating the code to return the generated {@link List} of {@link KeyParameter}s</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&key (c 3 c-p)) c)} is encountered:
	 * <pre>
	 * {@code
	 * protected List<KeyBinding> getKeyBindings() {
	 *      ArrayList var1 = new ArrayList();
	 *
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("C").getSymbol();
	 *      NILStruct var4 = NILStruct.INSTANCE;
	 *
	 *      var2 = PackageStruct.findPackage("KEYWORD");
	 *      SymbolStruct var5 = var2.intern("C").getSymbol();
	 *
	 *      var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var6 = var2.intern("C-P").getSymbol();
	 *      SuppliedPBinding var7 = new SuppliedPBinding(var6, false);
	 *
	 *      KeyBinding var8 = new KeyBinding(var3, var4, false, var5, var7);
	 *      var1.add(var8);
	 *
	 *      return var1;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@link List} of {@link KeyParameter}s to generate code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private void generateKeyBindings(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
	                                 final ClassWriter cw) {
		final List<KeyParameter> keyBindings = lambdaListBindings.getKeyBindings();
		if (keyBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_KEY_BINDINGS_METHOD_NAME,
		                                        GET_KEY_BINDINGS_METHOD_DESC,
		                                        GET_KEY_BINDINGS_METHOD_SIGNATURE,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

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

		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct keySymbol = keyBinding.getVar();
			CodeGenerators.generateSymbol(keySymbol, generatorState, keyPackageStore, keySymbolStore);

			NILStruct.INSTANCE.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);

			final SymbolStruct keyName = keyBinding.getKeyName();
			CodeGenerators.generateSymbol(keyName, generatorState, keyPackageStore, keyNameStore);

			final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				generateSuppliedPBinding(suppliedPBinding.get(), generatorState, keyPackageStore, keySuppliedPSymbolStore, keySuppliedPStore);
			} else {
				generateSuppliedPBinding(null, generatorState, keyPackageStore, keySuppliedPSymbolStore, keySuppliedPStore);

			}

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.KEY_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, keySymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyInitFormStore);
			if (keyBinding.isSpecial()) {
				generatorState.getDynamicSymbols().add(keySymbol);
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				generatorState.getLexicalSymbols().add(keySymbol);
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

	/**
	 * Private method for generating a {@link SuppliedPParameter} to be used in the {@link
	 * CompiledFunctionStruct#getOptionalBindings()} and {@link CompiledFunctionStruct#getKeyBindings()} method
	 * generation code as the {@link OptionalParameter#suppliedPBinding} and {@link KeyParameter#suppliedPBinding}
	 * values.
	 *
	 * @param suppliedPBinding
	 * 		the {@link SuppliedPParameter} to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param suppliedPPackageStore
	 * 		the storage location index on the stack where the {@link PackageStruct} for the provided {@link
	 * 		SuppliedPParameter#var} will exist
	 * @param suppliedPSymbolStore
	 * 		the storage location index on the stack where the {@link SuppliedPParameter#var} will exist
	 * @param suppliedPStore
	 * 		the storage location index on the stack where the generated {@link SuppliedPParameter} will exist
	 */
	private static void generateSuppliedPBinding(final SuppliedPParameter suppliedPBinding, final GeneratorState generatorState,
	                                             final int suppliedPPackageStore, final int suppliedPSymbolStore,
	                                             final int suppliedPStore) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (suppliedPBinding == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, suppliedPStore);
		} else {
			final SymbolStruct keySuppliedPSymbol = suppliedPBinding.getVar();
			CodeGenerators.generateSymbol(keySuppliedPSymbol, generatorState, suppliedPPackageStore, suppliedPSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SUPPLIED_P_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, suppliedPSymbolStore);
			if (suppliedPBinding.isSpecial()) {
				generatorState.getDynamicSymbols().add(keySuppliedPSymbol);
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				generatorState.getLexicalSymbols().add(keySuppliedPSymbol);
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

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getAllowOtherKeys()} ()} method for the
	 * generated lambda class object being written to via the provided {@link ClassWriter}. The generation will perform
	 * the following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the value of {@link
	 * OrdinaryLambdaList#allowOtherKeys} is false, as this is the default return value of the overridden
	 * method</li>
	 * <li>Generating and returning {@code true} the value of {@link OrdinaryLambdaList#allowOtherKeys} is
	 * true</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&allow-other-keys))} is encountered:
	 * <pre>
	 * {@code
	 * protected boolean getAllowOtherKeys() {
	 *      return true;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@literal &allow-other-keys} {@code boolean} value to
	 * 		generate code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private static void generateAllowOtherKeys(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
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

	/**
	 * Private method for generating the {@link CompiledFunctionStruct#getAuxBindings()} method for the generated
	 * lambda class object being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * OrdinaryLambdaList#auxBindings} is empty</li>
	 * <li>Generating the {@link List} for containing the resulting {@link AuxParameter}s</li>
	 * <li>Generating each of the {@link AuxParameter}s and adding them to the previously created {@link List}</li>
	 * <li>Generating the code to return the generated {@link List} of {@link AuxParameter}s</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (lambda (&aux (d 2)) d)} is encountered:
	 * <pre>
	 * {@code
	 * protected List<AuxBinding> getAuxBindings() {
	 *      ArrayList var1 = new ArrayList();
	 *
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("D").getSymbol();
	 *      AuxBinding var4 = new AuxBinding(var3, false);
	 *      var1.add(var4);
	 *
	 *      return var1;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param lambdaListBindings
	 * 		the {@link OrdinaryLambdaList} containing the {@link List} of {@link AuxParameter}s to generate code for
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private void generateAuxBindings(final GeneratorState generatorState, final OrdinaryLambdaList lambdaListBindings,
	                                 final ClassWriter cw) {
		final List<AuxParameter> auxBindings = lambdaListBindings.getAuxBindings();
		if (auxBindings.isEmpty()) {
			// No need to generate this method, as there are no bindings to generate
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GET_AUX_BINDINGS_METHOD_NAME,
		                                        GET_AUX_BINDINGS_METHOD_DESC,
		                                        GET_AUX_BINDINGS_METHOD_SIGNATURE,
		                                        null);

		final JavaEnvironmentMethodBuilder methodBuilder = new JavaEnvironmentMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();

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

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStruct auxSymbol = auxBinding.getVar();
			CodeGenerators.generateSymbol(auxSymbol, generatorState, auxPackageStore, auxSymbolStore);

			NILStruct.INSTANCE.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, auxInitFormStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.AUX_BINDING_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, auxSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxInitFormStore);
			if (auxBinding.isSpecial()) {
				generatorState.getDynamicSymbols().add(auxSymbol);
				mv.visitInsn(Opcodes.ICONST_1);
			} else {
				generatorState.getLexicalSymbols().add(auxSymbol);
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
