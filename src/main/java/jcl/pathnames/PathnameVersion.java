package jcl.pathnames;

import jcl.structs.conditions.exceptions.FileErrorException;

/**
 * The {@code PathnameVersion} is the object representation of the 'version' element of a Lisp 'pathname' type.
 */
public final class PathnameVersion {

	private final Integer version;
	private final PathnameVersionComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameVersion() {
		version = null;
		componentType = PathnameVersionComponentType.NEWEST;
	}

	/**
	 * Public constructor.
	 *
	 * @param version the pathname version
	 */
	public PathnameVersion(final Integer version) {
		if ((version == null) || (version < 1)) {
			throw new FileErrorException("Version value cannot be null or less than 1.");
		}

		this.version = version;
		componentType = PathnameVersionComponentType.NEWEST;
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType pathname version component type
	 */
	public PathnameVersion(final PathnameVersionComponentType componentType) {
		version = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname version value.
	 *
	 * @return pathname version value
	 */
	public Integer getVersion() {
		return version;
	}

	/**
	 * Getter for pathname version component type.
	 *
	 * @return pathname version component type
	 */
	public PathnameVersionComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return "PathnameVersion{"
				+ "version=" + version
				+ ", componentType=" + componentType
				+ '}';
	}
}
