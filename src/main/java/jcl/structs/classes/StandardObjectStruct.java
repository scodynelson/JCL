package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StandardObject;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct implements LispStruct {

	private String documentation;

	/**
	 * Public constructor.
	 */
	protected StandardObjectStruct() {
		documentation = null;
	}

	/**
	 * Getter for standard object documentation property.
	 *
	 * @return standard object documentation property
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Setter for standard object documentation property.
	 *
	 * @param documentation
	 * 		new standard object documentation property value
	 */
	public void setDocumentation(final String documentation) {
		this.documentation = documentation;
	}

	@Override
	public LispType getType() {
		return StandardObject.INSTANCE;
	}

	@Override
	public String printStruct() {
		final String typeClassName = getType().getClass().getName().toUpperCase();
		return "#<" + typeClassName + getPrintableObjectProperties() + '>';
	}

	/**
	 * Protected, overridable method to get printable object properties to use in the printStruct method.
	 *
	 * @return a string with printable object properties
	 */
	protected String getPrintableObjectProperties() {
		return "";
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
