/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import org.springframework.stereotype.Component;

@Component
public class FormGenerator implements CodeGenerator<LispStruct> {

	@Resource
	private Map<Class<? extends LispStruct>, CodeGenerator<LispStruct>> codeGeneratorStrategies;

	@Override
	public void generate(final LispStruct input, final JavaClassBuilder classBuilder) {

		final CodeGenerator<LispStruct> codeGenerator1 = codeGeneratorStrategies.get(input.getClass());
		if (codeGenerator1 == null) {
			throw new RuntimeException("ICG: Found thing I can't generate code for class: " + input.getClass().getName());
		}
		codeGenerator1.generate(input, classBuilder);
	}
}
