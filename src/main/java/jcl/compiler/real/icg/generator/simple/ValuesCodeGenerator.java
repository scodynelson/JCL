/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.ValuesStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ValuesCodeGenerator implements CodeGenerator<ValuesStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ValuesStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct value = input.getPrimaryValue();
		formGenerator.generate(value, classBuilder);
	}
}
