package jcl.pathnames;

/**
 * The {@code PathnameVersionComponentType} is the enumeration of the type of a component element of the version element
 * of a Lisp 'pathname' type.
 */
public enum PathnameVersionComponentType {
	UNSPECIFIC(":UNSPECIFIC"),
	WILD(":WILD"),
	NIL("NIL"),
	NEWEST(":NEWEST"),
	OLDEST(":OLDEST");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value value of the version component type
	 */
	PathnameVersionComponentType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for version component type value.
	 *
	 * @return version component type value
	 */
	public String getValue() {
		return value;
	}
}
