package jcl.functions.parameterdsl;

import java.util.Iterator;
import java.util.Map;

import jcl.LispStruct;

public class InitialValue {

	private final Parameter parameter;
	private final Map<String, LispStruct> parameterMap;
	private final String parameterName;
	private final Iterator<LispStruct> iterator;

	InitialValue(final Parameter parameter, final Map<String, LispStruct> parameterMap,
	                    final String parameterName, final Iterator<LispStruct> iterator) {
		this.parameter = parameter;
		this.parameterMap = parameterMap;
		this.parameterName = parameterName;
		this.iterator = iterator;
	}

	public Type withInitialValue(final LispStruct initialValue) {
		if (iterator.hasNext()) {
			return new Type(parameter, parameterMap, parameterName, iterator.next());
		} else {
			return new Type(parameter, parameterMap, parameterName, initialValue);
		}
	}
}
