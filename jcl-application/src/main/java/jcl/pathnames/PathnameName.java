/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameName} is the object representation of the 'name' element of a Lisp 'pathname' type.
 */
public final class PathnameName {

	/**
	 * The pathname name value.
	 */
	private final String name;

	/**
	 * The pathname name component type.
	 */
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameName() {
		name = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the pathname name
	 */
	public PathnameName(final String name) {
		this.name = name;

		if (StringUtils.isEmpty(name)) {
			componentType = PathnameComponentType.NIL;
		} else if ("*".equalsIgnoreCase(name)) {
			componentType = PathnameComponentType.WILD;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname name component type
	 */
	public PathnameName(final PathnameComponentType componentType) {
		name = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname name {@link #name} property.
	 *
	 * @return pathname name {@link #name} property
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for pathname name {@link #componentType} property.
	 *
	 * @return pathname name {@link #componentType} property
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(name)
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
		final PathnameName rhs = (PathnameName) obj;
		return new EqualsBuilder().append(name, rhs.name)
		                          .append(componentType, rhs.componentType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(name)
		                                                                .append(componentType)
		                                                                .toString();
	}
}
