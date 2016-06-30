/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform the generation of the code for anonymous lambda function objects, such as '#'(lambda ())'.
 */
@Component
final class LambdaFunctionCodeGenerator implements CodeGenerator<LambdaCompilerFunctionStruct> {

	/**
	 * {@link LambdaCodeGenerator} used for generating the {@link LambdaCompilerFunctionStruct#lambdaStruct} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

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
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<LambdaCompilerFunctionStruct> event) {
		final LambdaCompilerFunctionStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final LambdaStruct lambdaStruct = input.getLambdaStruct();
		codeGenerator.generate(lambdaStruct, generatorState);
	}
}
