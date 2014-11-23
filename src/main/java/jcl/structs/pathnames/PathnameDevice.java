/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.pathnames;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDevice} is the object representation of the 'device' element of a Lisp 'pathname' type.
 */
public final class PathnameDevice {

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
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
