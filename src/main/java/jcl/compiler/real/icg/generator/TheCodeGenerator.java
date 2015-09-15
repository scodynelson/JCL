/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.struct.specialoperator.TheStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'the' special operator code generation.
 */
@Component
class TheCodeGenerator implements CodeGenerator<TheStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link TheStruct#form} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link TheStruct} objects. The {@link TheStruct#form} value is passed directly to the
	 * {@link IntermediateCodeGenerator} to be generated.
	 *
	 * @param input
	 * 		the {@link TheStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final TheStruct input, final GeneratorState generatorState) {
		// TODO: do we want to add the logic here to verify the type information of the generated form???

		final LispStruct form = input.getForm();
		codeGenerator.generate(form, generatorState);
	}
}
