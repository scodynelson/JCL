/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname'
 * type.
 */
public enum PathnameDirectoryType {

	/**
	 * Absolute pathname directory type.
	 */
	ABSOLUTE(":ABSOLUTE"),

	/**
	 * Relative pathname directory type.
	 */
	RELATIVE(":RELATIVE");

	/**
	 * String value of the pathname directory type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory type
	 */
	PathnameDirectoryType(final String value) {
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
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
