/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.LambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaFunctionCodeGenerator implements CodeGenerator<LambdaCompilerFunctionStruct> {

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final LambdaCompilerFunctionStruct input, final JavaClassBuilder classBuilder) {

		final LambdaStruct lambdaStruct = input.getLambdaStruct();
		lambdaCodeGenerator.generate(lambdaStruct, classBuilder);
	}
}
