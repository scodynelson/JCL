package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname'
 * type.
 */
public enum PathnameDirectoryType {

	ABSOLUTE(":ABSOLUTE"),
	RELATIVE(":RELATIVE");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory type
	 */
	PathnameDirectoryType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for directory type value.
	 *
	 * @return directory type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
