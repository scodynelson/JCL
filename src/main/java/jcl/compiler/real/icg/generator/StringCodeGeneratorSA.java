/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.arrays.StringStruct;
import jcl.compiler.real.icg.GeneratorState;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StringCodeGeneratorSA {

	@Autowired
	private StringCodeGenerator stringCodeGenerator;

	public GeneratorState generate(final StringStruct input, final GeneratorState generatorState) {
		stringCodeGenerator.generate(input, generatorState);
		return generatorState;
	}
}
