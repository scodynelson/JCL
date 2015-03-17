/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameStructType} is the enumeration of the structure type of a Lisp 'pathname' type.
 */
public enum PathnameStructType {

	/**
	 * File pathname structure type.
	 */
	FILE(":FILE"),

	/**
	 * URI pathname structure type.
	 */
	URI(":URI");

	/**
	 * String value of the pathname structure type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the pathname structure type
	 */
	PathnameStructType(final String value) {
		this.value = value;
	}

	/**
	 * Getter for pathname structure type value.
	 *
	 * @return pathname structure type value
	 */
	public String getValue() {
		return value;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
