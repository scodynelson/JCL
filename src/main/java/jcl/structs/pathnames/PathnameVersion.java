package jcl.structs.pathnames;

import jcl.structs.conditions.exceptions.FileErrorException;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameVersion} is the object representation of the 'version' element of a Lisp 'pathname' type.
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
	 * @param version
	 * 		the pathname version
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
	 * @param componentType
	 * 		pathname version component type
	 */
	public PathnameVersion(final PathnameVersionComponentType componentType) {
		version = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname version {@link #version} property.
	 *
	 * @return pathname version {@link #version} property
	 */
	public Integer getVersion() {
		return version;
	}

	/**
	 * Getter for pathname version {@link #componentType} property.
	 *
	 * @return pathname version {@link #componentType} property
	 */
	public PathnameVersionComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
