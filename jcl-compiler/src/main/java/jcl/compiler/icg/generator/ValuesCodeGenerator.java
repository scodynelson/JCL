/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ValuesStruct} objects dynamically by generating the {@link ValuesStruct#getPrimaryValue()}
 * value.
 */
@Component
final class ValuesCodeGenerator implements CodeGenerator<ValuesStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ValuesStruct#getPrimaryValue()} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ValuesStruct} objects, by generating the {@link ValuesStruct#getPrimaryValue()}
	 * value.
	 *
	 * @param input
	 * 		the {@link ValuesStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<ValuesStruct> event) {
		final ValuesStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final LispStruct value = input.getPrimaryValue();
		codeGenerator.generate(value, generatorState);
	}
}
