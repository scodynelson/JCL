package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StandardObject;

/**
 * The {@code StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct implements LispStruct {

	private String documentation;

	/**
	 * Public constructor.
	 */
	protected StandardObjectStruct() {
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
