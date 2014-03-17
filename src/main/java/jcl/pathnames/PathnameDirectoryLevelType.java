package jcl.pathnames;

/**
 * The {@code PathnameDirectoryLevelType} is the enumeration of the directory level type of a 'directory' level
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
	 * @param value value of the directory level type
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
}
