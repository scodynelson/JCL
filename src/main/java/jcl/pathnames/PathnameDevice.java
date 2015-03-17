/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.Serializable;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDevice} is the object representation of the 'device' element of a Lisp 'pathname' type.
 */
public final class PathnameDevice implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2441578904423559720L;

	/**
	 * The pathname device value.
	 */
	private final String device;

	/**
	 * The pathname device component type.
	 */
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameDevice() {
		device = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param device
	 * 		the pathname device
	 */
	public PathnameDevice(final String device) {
		this.device = device;

		if (StringUtils.isEmpty(device)) {
			componentType = PathnameComponentType.NIL;
		} else if ("*".equals(device)) {
			componentType = PathnameComponentType.WILD;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname device component type
	 */
	public PathnameDevice(final PathnameComponentType componentType) {
		device = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname device {@link #device} property.
	 *
	 * @return pathname device {@link #device} property
	 */
	public String getDevice() {
		return device;
	}

	/**
	 * Getter for pathname device {@link #componentType} property.
	 *
	 * @return pathname device {@link #componentType} property
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
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
