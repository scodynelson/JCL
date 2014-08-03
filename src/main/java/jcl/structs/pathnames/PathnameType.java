package jcl.structs.pathnames;

import org.apache.commons.lang3.StringUtils;

/**
 * The {@link PathnameType} is the object representation of the 'type' element of a Lisp 'pathname' type.
 */
public final class PathnameType {

	private final String type;
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameType() {
		type = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param type the pathname type
	 */
	public PathnameType(final String type) {
		this.type = type;

		if (StringUtils.isEmpty(type)) {
			componentType = PathnameComponentType.NIL;
		} else if ("*".equalsIgnoreCase(type)) {
			componentType = PathnameComponentType.WILD;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType pathname type component type
	 */
	public PathnameType(final PathnameComponentType componentType) {
		type = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname type value.
	 *
	 * @return pathname type value
	 */
	public String getType() {
		return type;
	}

	/**
	 * Getter for pathname type component type.
	 *
	 * @return pathname type component type
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return "PathnameType{"
				+ "type=" + type
				+ ", componentType=" + componentType
				+ '}';
	}
}
