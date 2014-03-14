package jcl.pathnames;

/**
 * The {@code PathnameCase} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
 */
public enum PathnameCaseType {
	COMMON(":COMMON"),
	LOCAL(":LOCAL");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value value of the case type
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
}
