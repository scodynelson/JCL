/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.LispStruct;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.struct.specialoperator.TheStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'the' special operator code generation.
 */
@Component
final class TheCodeGenerator implements CodeGenerator<TheStruct> {

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
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<TheStruct> event) {
		final TheStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();
		// TODO: do we want to add the logic here to verify the type information of the generated form???

		final LispStruct form = input.getForm();
		codeGenerator.generate(form, generatorState);
	}
}
