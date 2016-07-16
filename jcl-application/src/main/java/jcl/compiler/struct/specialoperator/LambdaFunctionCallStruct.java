/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class LambdaFunctionCallStruct extends CompilerSpecialOperatorStruct {

	private final LambdaCompilerFunctionStruct lambdaCompilerFunction;

	private final List<LispStruct> arguments;

	public LambdaFunctionCallStruct(final LambdaCompilerFunctionStruct lambdaCompilerFunction, final List<LispStruct> arguments) {
		this.lambdaCompilerFunction = lambdaCompilerFunction;
		this.arguments = arguments;
	}

	public LambdaCompilerFunctionStruct getLambdaCompilerFunction() {
		return lambdaCompilerFunction;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}
}
