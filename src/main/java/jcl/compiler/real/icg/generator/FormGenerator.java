/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import org.springframework.stereotype.Component;

@Component
public class FormGenerator implements CodeGenerator<LispStruct> {

	@Resource
	private Map<Class<? extends LispStruct>, CodeGenerator<LispStruct>> codeGeneratorStrategies;

	@Override
	public void generate(final LispStruct input, final GeneratorState generatorState) {

		final CodeGenerator<LispStruct> codeGenerator = codeGeneratorStrategies.get(input.getClass());
		if (codeGenerator == null) {
			throw new RuntimeException("ICG: Found thing I can't generate code for class: " + input.getClass().getName());
		}
		codeGenerator.generate(input, generatorState);
	}
}
