/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaCodeGeneratorSA {

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	public GeneratorState generate(final LambdaStruct input, final GeneratorState generatorState) {
		lambdaCodeGenerator.generate(input, generatorState);
		return generatorState;
	}
}
