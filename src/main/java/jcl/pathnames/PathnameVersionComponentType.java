/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameVersionComponentType} is the enumeration of the type of a component element of the version
 * element of a Lisp 'pathname' type.
 */
public enum PathnameVersionComponentType {

	/**
	 * Unspecific pathname version component type.
	 */
	UNSPECIFIC(":UNSPECIFIC"),

	/**
	 * Wild pathname version component type.
	 */
	WILD(":WILD"),

	/**
	 * Nil pathname version component type.
	 */
	NIL("NIL"),

	/**
	 * Newest pathname version component type.
	 */
	NEWEST(":NEWEST"),

	/**
	 * Oldest pathname version component type.
	 */
	OLDEST(":OLDEST");

	/**
	 * String value of the pathname version component type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the version component type
	 */
	PathnameVersionComponentType(final String value) {
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
