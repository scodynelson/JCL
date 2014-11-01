package jcl.structs.classes;

import jcl.LispType;
import jcl.types.StandardMethod;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StandardMethodStruct} is the object representation of a Lisp 'standard-method' type.
 */
public abstract class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	@Override
	public LispType getType() {
		return StandardMethod.INSTANCE;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
