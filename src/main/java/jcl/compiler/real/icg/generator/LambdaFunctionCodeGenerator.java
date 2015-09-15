/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform the generation of the code for anonymous lambda function objects, such as '#'(lambda ())'.
 */
@Component
class LambdaFunctionCodeGenerator implements CodeGenerator<LambdaCompilerFunctionStruct> {

	/**
	 * {@link LambdaCodeGenerator} used for generating the {@link LambdaCompilerFunctionStruct#lambdaStruct} value.
	 */
	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LambdaCompilerFunctionStruct} objects, by performing the following operations:
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
	 * @param input
	 * 		the {@link LambdaCompilerFunctionStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final LambdaCompilerFunctionStruct input, final GeneratorState generatorState) {

		final LambdaStruct lambdaStruct = input.getLambdaStruct();
		lambdaCodeGenerator.generate(lambdaStruct, generatorState);
	}
}
