package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameStructType} is the enumeration of the structure type of a Lisp 'pathname' type.
 */
public enum PathnameStructType {

	FILE(":FILE"),
	URI(":URI");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the pathname structure type
	 */
	PathnameStructType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for pathname structure type value.
	 *
	 * @return pathname structure type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
