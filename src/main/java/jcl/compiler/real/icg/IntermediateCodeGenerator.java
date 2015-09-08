/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import java.util.Deque;

import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;

public interface IntermediateCodeGenerator {

	Deque<JavaClassBuilder> generate(LambdaStruct lambdaStruct);
}
