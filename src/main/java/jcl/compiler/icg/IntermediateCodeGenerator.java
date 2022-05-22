/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import java.util.Deque;

import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.LispStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class IntermediateCodeGenerator {

	public static Deque<JavaClassBuilder> generate(final LambdaStruct lambdaStruct) {
		final GeneratorState classBuilder = new GeneratorState();
		generate(lambdaStruct, classBuilder);
		return classBuilder.getFinalClassBuilderDeque();
	}

	public static void generate(final LispStruct input, final GeneratorState generatorState) {
		input.generate(generatorState);
	}
}
