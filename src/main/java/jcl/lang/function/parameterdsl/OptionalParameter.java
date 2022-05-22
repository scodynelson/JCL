package jcl.lang.function.parameterdsl;

import jcl.lang.LispStruct;

public class OptionalParameter {

	private final Parameters parameters;
	private final String parameterName;
	private LispStruct initialValue;

	OptionalParameter(final Parameters parameters, final String parameterName) {
		this.parameters = parameters;
		this.parameterName = parameterName;
	}

	public Parameters and() {
		return parameters;
	}

	public Parameters withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return parameters;
	}

	String getParameterName() {
		return parameterName;
	}

	LispStruct getInitialValue() {
		return initialValue;
	}
}
