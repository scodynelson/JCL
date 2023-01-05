/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.struct.specialoperator.lambda.MacroLambdaStruct;
import jcl.lang.LispStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Class to perform the generation of the code for anonymous lambda function objects, such as '#'(lambda ())'.
 */
@Getter
@AllArgsConstructor
public class MacroLambdaCompilerFunctionStruct implements CompilerFunctionStruct {

	private final MacroLambdaStruct macroLambdaStruct;

	/**
	 * {@inheritDoc} Generation method for {@code MacroLambdaCompilerFunctionStruct} objects, by performing the
	 * following operations:
	 * <ol>
	 * <li>Generating the {@link MacroLambdaCompilerFunctionStruct#macroLambdaStruct} value, creating the anonymous {@link
	 * MacroLambdaStruct} class</li>
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
		macroLambdaStruct.generate(generatorState);
	}

	@Override
	public LispStruct eval(final Environment environment) {
		// TODO: is this correct??
		return this;
	}
}
