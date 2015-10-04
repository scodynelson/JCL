/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.compiler.real.icg.GeneratorState;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.Router;

@MessageEndpoint
public class ICGRouter {

	@Autowired
	private ArrayCodeGenerator arrayCodeGenerator;

	@Router(inputChannel = "generationChannel")
	public GeneratorState route(final ArrayStruct<LispStruct> input) {
//		final ArrayStruct<LispStruct> payload = (ArrayStruct<LispStruct>) input.getPayload();
		final GeneratorState generatorState = null;

		arrayCodeGenerator.generate(input, generatorState);
		return generatorState;
	}
}
