package jcl.functions.parameterdsl;

import jcl.LispStruct;

public class Required {

	private final Parameters parameters;
	private final String parameterName;
	private Class<? extends LispStruct> clazz;

	Required(final Parameters parameters, final String parameterName) {
		this.parameters = parameters;
		this.parameterName = parameterName;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	String getParameterName() {
		return parameterName;
	}

	Class<? extends LispStruct> getClazz() {
		return clazz;
	}
}
