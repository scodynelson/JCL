/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import java.util.Deque;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;

public interface IntermediateCodeGenerator {

	Deque<JavaClassBuilder> generate(LambdaStruct lambdaStruct);

	void generate(LispStruct input, GeneratorState generatorState);
}
