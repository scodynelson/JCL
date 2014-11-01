package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryLevelType} is the enumeration of the directory level type of a 'directory' level
 * element of a Lisp 'pathname' type.
 * NOTE: This implementation does NOT support WildInferiors. Period.
 */
public enum PathnameDirectoryLevelType {

	WILD(":WILD"),
	BACK(":BACK"),
	UP(":UP"),
	NULL(null);

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory level type
	 */
	PathnameDirectoryLevelType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for directory level type value.
	 *
	 * @return directory level type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
