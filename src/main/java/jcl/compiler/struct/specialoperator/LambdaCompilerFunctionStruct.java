/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;

public class LambdaCompilerFunctionStruct implements CompilerFunctionStruct {

	private final LambdaStruct lambdaStruct;

	public LambdaCompilerFunctionStruct(final LambdaStruct lambdaStruct) {
		this.lambdaStruct = lambdaStruct;
	}

	public LambdaStruct getLambdaStruct() {
		return lambdaStruct;
	}
}
