package jcl.functions.parameterdsl;

import jcl.LispStruct;
import jcl.symbols.NILStruct;

public class Optional {

	private final Parameters parameters;
	private final String parameterName;
	private LispStruct initialValue = NILStruct.INSTANCE;
	private Class<? extends LispStruct> clazz;

	Optional(final Parameters parameters, final String parameterName) {
		this.parameters = parameters;
		this.parameterName = parameterName;
	}

	public Optional withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return this;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	String getParameterName() {
		return parameterName;
	}

	LispStruct getInitialValue() {
		return initialValue;
	}

	Class<? extends LispStruct> getClazz() {
		return clazz;
	}
}
