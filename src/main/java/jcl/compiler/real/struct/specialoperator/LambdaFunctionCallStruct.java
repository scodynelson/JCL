/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class LambdaFunctionCallStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -6330612245404973713L;

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
