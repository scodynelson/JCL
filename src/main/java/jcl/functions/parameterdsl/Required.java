package jcl.functions.parameterdsl;

import jcl.LispStruct;

public class Required {

	private final String parameterName;
	private final Parameters parameters;
	private Class<?> clazz;

	Required(final String parameterName, final Parameters parameters) {
		this.parameterName = parameterName;
		this.parameters = parameters;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	String getParameterName() {
		return parameterName;
	}

	Class<?> getClazz() {
		return clazz;
	}
}
