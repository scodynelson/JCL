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

@Component
class ValuesCodeGenerator implements CodeGenerator<ValuesStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Override
	public void generate(final ValuesStruct input, final GeneratorState generatorState) {

		final LispStruct value = input.getPrimaryValue();
		codeGenerator.generate(value, generatorState);
	}
}
