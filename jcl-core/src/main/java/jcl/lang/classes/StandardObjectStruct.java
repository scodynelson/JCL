package jcl.lang.classes;

import jcl.lang.LispStruct;
import jcl.type.LispType;
import jcl.type.StandardObjectType;

/**
 * The {@link StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct implements LispStruct {

	protected String documentation;

	/**
	 * Protected constructor.
	 */
	protected StandardObjectStruct() {
		documentation = null;
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 */
	protected StandardObjectStruct(final String documentation) {
		this.documentation = documentation;
	}

	/**
	 * Getter for standard object {@link #documentation} property.
	 *
	 * @return standard object {@link #documentation} property
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Setter for standard object {@link #documentation} property.
	 *
	 * @param documentation
	 * 		new standard object {@link #documentation} property value
	 */
	public void setDocumentation(final String documentation) {
		this.documentation = documentation;
	}

	@Override
	public LispType getType() {
		return StandardObjectType.INSTANCE;
	}

	@Override
	public String toString() {
		final String typeClassName = getClass().getSimpleName();
		return "#<" + typeClassName + '>';
	}
}
