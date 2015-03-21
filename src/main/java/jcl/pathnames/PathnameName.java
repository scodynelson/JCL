/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.Serializable;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameName} is the object representation of the 'name' element of a Lisp 'pathname' type.
 */
public final class PathnameName implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8569658491700316910L;

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
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
