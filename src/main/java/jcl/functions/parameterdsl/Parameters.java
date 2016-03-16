package jcl.functions.parameterdsl;

import java.util.List;

import jcl.LispStruct;

public class Parameters {

	private final FunctionParameters functionParam;

	private Parameters(final FunctionParameters functionParam) {
		this.functionParam = functionParam;
	}

	public static Parameters forFunction(final String functionName) {
		final FunctionParameters functionParam = new FunctionParameters(functionName);
		return new Parameters(functionParam);
	}

	public Parameter withParameters(final List<LispStruct> parameters) {
		return new Parameter(functionParam, parameters);
	}
}
