package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameCaseType} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
 */
public enum PathnameCaseType {

	COMMON(":COMMON"),
	LOCAL(":LOCAL");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the case type
	 */
	PathnameCaseType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for case type value.
	 *
	 * @return case type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
