/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.struct.ValuesStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ValuesStruct} objects dynamically by generating the {@link ValuesStruct#getPrimaryValue()}
 * value.
 */
@Component
class ValuesCodeGenerator implements CodeGenerator<ValuesStruct> {

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
	@Override
	public void generate(final ValuesStruct input, final GeneratorState generatorState) {

		final LispStruct value = input.getPrimaryValue();
		codeGenerator.generate(value, generatorState);
	}
}
