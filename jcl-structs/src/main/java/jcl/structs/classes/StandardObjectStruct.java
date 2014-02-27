package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.StandardObject;

/**
 * The {@code StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public class StandardObjectStruct implements LispStruct {

	private String documentation;

	/**
	 * Public constructor.
	 */
	public StandardObjectStruct() {
		documentation = null;
	}

	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(final String documentation) {
		this.documentation = documentation;
	}

	@Override
	public LispType getType() {
		return StandardObject.INSTANCE;
	}

	@Override
	public String toString() {
		return "StandardObjectStruct{"
				+ "documentation='" + documentation + '\''
				+ '}';
	}
}
