package jcl.functions.parameterdsl;

import jcl.LispStruct;
import jcl.symbols.NILStruct;

public class Optional {

	private final String parameterName;
	private final Parameters parameters;
	private Class<?> clazz;
	private LispStruct initialValue = NILStruct.INSTANCE;

	Optional(final String parameterName, final Parameters parameters) {
		this.parameterName = parameterName;
		this.parameters = parameters;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	public Optional withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return this;
	}

	String getParameterName() {
		return parameterName;
	}

	Class<?> getClazz() {
		return clazz;
	}
}
