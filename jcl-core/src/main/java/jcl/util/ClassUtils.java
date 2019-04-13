package jcl.util;

import jcl.lang.condition.exception.TypeErrorException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ClassUtils {

	@SuppressWarnings("unchecked")
	@Deprecated
	public static <T> T convert(final Object value, final Class<T> type) {
		if (org.springframework.util.ClassUtils.isAssignableValue(type, value)) {
			return (T) value;
		}
		throw new TypeErrorException("Cannot convert value '" + value + "' to type '" + type.getSimpleName() + '\'');
	}
}
