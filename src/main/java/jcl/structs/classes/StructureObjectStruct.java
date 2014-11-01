package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StructureObject;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
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
