/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameType} is the object representation of the 'type' element of a Lisp 'pathname' type.
 */
public final class PathnameType {

	/**
	 * The pathname type value.
	 */
	private final String type;

	/**
	 * The pathname type component type.
	 */
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
	 * @param type
	 * 		the pathname type
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
	 * @param componentType
	 * 		pathname type component type
	 */
	public PathnameType(final PathnameComponentType componentType) {
		type = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname type {@link #type} property.
	 *
	 * @return pathname type {@link #type} property
	 */
	public String getType() {
		return type;
	}

	/**
	 * Getter for pathname type {@link #componentType} property.
	 *
	 * @return pathname type {@link #componentType} property
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
