package jcl.util;

import jcl.conditions.exceptions.TypeErrorException;

public final class ClassUtils {

	private ClassUtils() {
	}

	@SuppressWarnings("unchecked")
	public static <T> T convert(final Class<T> type, final Object value) {
		if (org.springframework.util.ClassUtils.isAssignableValue(type, value)) {
			return (T) value;
		}
		throw new TypeErrorException("Cannot convert value '" + value + "' to type '" + type.getName() + '\'');
	}
}
