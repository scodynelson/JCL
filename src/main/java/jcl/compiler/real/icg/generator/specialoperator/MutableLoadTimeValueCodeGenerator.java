/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MutableLoadTimeValueCodeGenerator implements CodeGenerator<MutableLoadTimeValueStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final MutableLoadTimeValueStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct form = input.getForm();
		formGenerator.generate(form, classBuilder);
	}
}
