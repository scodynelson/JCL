package jcl.functions.parameterdsl;

import java.util.Map;

import jcl.LispStruct;
import jcl.util.ClassUtils;

public class Type {

	private final Parameter parameter;
	private final Map<String, LispStruct> parameterMap;
	private final String parameterName;
	private final LispStruct value;

	Type(final Parameter parameter, final Map<String, LispStruct> parameterMap,
	            final String parameterName, final LispStruct value) {
		this.parameter = parameter;
		this.parameterMap = parameterMap;
		this.parameterName = parameterName;
		this.value = value;
	}

	public <T extends LispStruct> Parameter as(final Class<T> clazz) {
		final T convert = ClassUtils.convert(clazz, value);
		parameterMap.put(parameterName, convert);
		return parameter;
	}
}
