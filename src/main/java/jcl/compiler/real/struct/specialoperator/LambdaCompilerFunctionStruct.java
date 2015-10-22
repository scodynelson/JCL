/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;

public class LambdaCompilerFunctionStruct implements CompilerFunctionStruct {

	private static final long serialVersionUID = 1418688382783925560L;

	private final LambdaStruct lambdaStruct;

	public LambdaCompilerFunctionStruct(final LambdaStruct lambdaStruct) {
		this.lambdaStruct = lambdaStruct;
	}

	public LambdaStruct getLambdaStruct() {
		return lambdaStruct;
	}
}
