package jcl.structs.pathnames;

/**
 * The {@link PathnameComponentType} is the enumeration of the type of a component element of a Lisp 'pathname' type.
 * TODO: support both "wild" singular and plural. right now we only support plural: '*' vs '?'
 * TODO: also for UNIX, support character groupings and negation
 * NOTE: should 'WILD' things eventually be known as 'GLOB' things???
 * http://en.wikipedia.org/wiki/Glob_(programming)
 */
public enum PathnameComponentType {

	UNSPECIFIC(":UNSPECIFIC"),
	WILD(":WILD"),
	NIL("NIL");

	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value value of the component type
	 */
	PathnameComponentType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for component type value.
	 *
	 * @return component type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "PathnameComponentType{"
				+ "value='" + value + '\''
				+ '}';
	}
}
