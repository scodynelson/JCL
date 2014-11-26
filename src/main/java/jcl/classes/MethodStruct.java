package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.Method;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link MethodStruct} is the object representation of a Lisp 'method' type.
 */
public abstract class MethodStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Method.INSTANCE;
	}

	@Override
	public String printStruct() {
		final String typeClassName = getType().getClass().getName().toUpperCase();
		return "#<" + typeClassName + '>';
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}