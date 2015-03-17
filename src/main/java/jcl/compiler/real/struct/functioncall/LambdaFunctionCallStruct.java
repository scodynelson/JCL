/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.functioncall;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;

public class LambdaFunctionCallStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -6330612245404973713L;

	private final LambdaStruct lambdaStruct;

	private final List<LispStruct> arguments;

	public LambdaFunctionCallStruct(final LambdaStruct lambdaStruct, final List<LispStruct> arguments) {
		this.lambdaStruct = lambdaStruct;
		this.arguments = arguments;
	}

	public LambdaStruct getLambdaStruct() {
		return lambdaStruct;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}
}
