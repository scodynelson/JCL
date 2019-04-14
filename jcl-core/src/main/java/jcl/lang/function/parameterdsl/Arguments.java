package jcl.lang.function.parameterdsl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.TypeErrorException;

import static java.util.stream.Collectors.toList;

public final class Arguments {

	private Map<String, LispStruct> requiredParameters;
	private Map<String, LispStruct> optionalParameters;
	private List<LispStruct> restParameter;
	private Map<KeywordStruct, LispStruct> keyParameters;

	Arguments() {
	}

	Map<String, LispStruct> getRequiredParameters() {
		if (requiredParameters == null) {
			requiredParameters = new HashMap<>();
		}
		return requiredParameters;
	}

	public LispStruct getRequiredArgument(final String parameterName) {
		return getRequiredParameters().get(parameterName);
	}

	public <T extends LispStruct> T getRequiredArgument(final String parameterName, final Class<T> clazz) {
		final LispStruct parameterValue = getRequiredParameters().get(parameterName);
		return convert(parameterValue, clazz);
	}

	Map<String, LispStruct> getOptionalParameters() {
		if (optionalParameters == null) {
			optionalParameters = new HashMap<>();
		}
		return optionalParameters;
	}

	public LispStruct getOptionalArgument(final String parameterName) {
		return getOptionalParameters().get(parameterName);
	}

	public boolean hasOptionalArgument(final String parameterName) {
		if (optionalParameters == null) {
			return false;
		}
		return optionalParameters.containsKey(parameterName);
	}

	public <T extends LispStruct> T getOptionalArgument(final String parameterName, final Class<T> clazz) {
		final LispStruct parameterValue = getOptionalParameters().get(parameterName);
		return convert(parameterValue, clazz);
	}

	List<LispStruct> getRestParameter() {
		if (restParameter == null) {
			restParameter = new ArrayList<>();
		}
		return restParameter;
	}

	public List<LispStruct> getRestArgument() {
		return getRestParameter();
	}

	public <T extends LispStruct> List<T> getRestArgument(final Class<T> clazz) {
		return getRestParameter().stream()
		                         .map(parameterValue -> convert(parameterValue, clazz))
		                         .collect(toList());
	}

	Map<KeywordStruct, LispStruct> getKeyParameters() {
		if (keyParameters == null) {
			keyParameters = new HashMap<>();
		}
		return keyParameters;
	}

	public LispStruct getKeyArgument(final KeywordStruct keyword) {
		return getKeyParameters().get(keyword);
	}

	public boolean hasKeyArgument(final KeywordStruct keyword) {
		if (keyParameters == null) {
			return false;
		}
		return keyParameters.containsKey(keyword);
	}

	public <T extends LispStruct> T getKeyArgument(final KeywordStruct keyword, final Class<T> clazz) {
		final LispStruct parameterValue = getKeyParameters().get(keyword);
		return convert(parameterValue, clazz);
	}

	@SuppressWarnings("unchecked")
	private static <T> T convert(final Object value, final Class<T> type) {
		if (value == null) {
			return null;
		}
		if (type.isAssignableFrom(value.getClass())) {
			return (T) value;
		}
		throw new TypeErrorException("Cannot convert value '" + value + "' to type '" + type.getSimpleName() + '\'');
	}
}
