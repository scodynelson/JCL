/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.CompileForm;
import jcl.compiler.function.CompileResult;
import jcl.compiler.function.InternalEval;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Class to perform the generation of the code for anonymous lambda function calls, such as '((lambda ()))'.
 */
@Getter
public class LambdaFunctionCallStruct extends CompilerSpecialOperatorStruct {

	private final LambdaCompilerFunctionStruct lambdaCompilerFunction;
	private final List<LispStruct> arguments;

	public LambdaFunctionCallStruct(final LambdaCompilerFunctionStruct lambdaCompilerFunction, final List<LispStruct> arguments) {
		super("lambdaFunctionCall");
		this.lambdaCompilerFunction = lambdaCompilerFunction;
		this.arguments = arguments;
	}

	@Override
	public LispStruct eval(final Environment environment) {
		final LambdaStruct lambda = lambdaCompilerFunction.getLambdaStruct();

		final CompileResult compileResult = CompileForm.compile(lambda);
		final FunctionStruct function = compileResult.getFunction();

		final List<LispStruct> evaluatedArguments = new ArrayList<>(arguments.size());
		for (final LispStruct argument : arguments) {
			final LispStruct evaluatedArgument = InternalEval.eval(argument);
			evaluatedArguments.add(evaluatedArgument);
		}

		final LispStruct[] args = new LispStruct[evaluatedArguments.size()];
		evaluatedArguments.toArray(args);

		// NOTE: This cast should be safe since we're compiling a lambda form. If it doesn't cast, we have a bigger problem somewhere.
		final FunctionStruct compiledLambda = (FunctionStruct) function.apply();
		return compiledLambda.apply(args);
	}

	/**
	 * {@inheritDoc} Generation method for {@code LambdaFunctionCallStruct} objects, by performing the following
	 * operations:
	 * <ol>
	 * <li>Generating the {@link LambdaFunctionCallStruct#lambdaCompilerFunction} value, creating the anonymous {@link
	 * LambdaStruct} class</li>
	 * <li>Generating a new array of {@link LispStruct} to be used as the function call arguments</li>
	 * <li>Generating each of the {@link LambdaFunctionCallStruct#arguments} values and adding them to the arguments
	 * array</li>
	 * <li>Generating the code to invoke the anonymous function call passing the created array of arguments</li>
	 * </ol>
	 * As an example, it will transform the anonymous lambda function call for {@code ((lambda ()))} into the following
	 * Java code:
	 * <pre>
	 * {@code
	 * private LispStruct lambdaFunctionCall_1(Environment var1) {
	 *      Lambda_10 var2 = new Lambda_10(var1);
	 *      LispStruct[] var3 = new LispStruct[0];
	 *      return var2.apply(var3);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 *        {@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		lambdaCompilerFunction.generate(generatorState);

		final int functionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionStore);

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);

		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();
		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			argument.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.FUNCTION_STRUCT_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
		                   true);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
