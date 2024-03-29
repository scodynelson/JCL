/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.CompileForm;
import jcl.compiler.function.CompileResult;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Class to perform the generation of the code for anonymous lambda function objects, such as '#'(lambda ())'.
 */
@Getter
@AllArgsConstructor
public class LambdaCompilerFunctionStruct implements CompilerFunctionStruct {

	private final LambdaStruct lambdaStruct;

	/**
	 * {@inheritDoc} Generation method for {@code LambdaCompilerFunctionStruct} objects, by performing the following
	 * operations:
	 * <ol>
	 * <li>Generating the {@link LambdaCompilerFunctionStruct#lambdaStruct} value, creating the anonymous {@link
	 * LambdaStruct} class</li>
	 * </ol>
	 * As an example, it will transform the anonymous lambda function for {@code (lambda ())} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 *      new Lambda_10(var1);
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		lambdaStruct.generate(generatorState);
	}

	@Override
	public LispStruct eval(final Environment environment) {
		final CompileResult compileResult = CompileForm.compile(lambdaStruct);
		final FunctionStruct function = compileResult.getFunction();
		return function.apply();
	}

	@Override
	public String toString() {
		return "(FUNCTION " + lambdaStruct.toString() + ')';
	}
}
