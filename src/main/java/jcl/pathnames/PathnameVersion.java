/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.Serializable;

import jcl.conditions.exceptions.FileErrorException;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameVersion} is the object representation of the 'version' element of a Lisp 'pathname' type.
 */
public final class PathnameVersion implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7874975312401979568L;

	/**
	 * The pathname version value.
	 */
	private final Integer version;

	/**
	 * The pathname version component type.
	 */
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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(version)
		                            .append(componentType)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final PathnameVersion rhs = (PathnameVersion) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(version, rhs.version)
		                          .append(componentType, rhs.componentType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(version)
		                                                                .append(componentType)
		                                                                .toString();
	}
}
