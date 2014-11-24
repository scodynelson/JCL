/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameCaseType} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
 */
public enum PathnameCaseType {

	/**
	 * Unspecific pathname case type.
	 */
	COMMON(":COMMON"),

	/**
	 * Unspecific pathname case type.
	 */
	LOCAL(":LOCAL");

	/**
	 * String value of the pathname case type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the case type
	 */
	PathnameCaseType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for {@link #value} property.
	 *
	 * @return {@link #value} property
	 */
	public String getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
