package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StructureObject;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	private static final long serialVersionUID = 5766790087319221572L;

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
