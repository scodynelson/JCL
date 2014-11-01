package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.MethodCombination;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link MethodCombinationStruct} is the object representation of a Lisp 'method-combination' type.
 */
public abstract class MethodCombinationStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodCombination.INSTANCE;
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
