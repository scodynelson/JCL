/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameComponentType} is the enumeration of the type of a component element of a Lisp 'pathname' type.
 * TODO: support both "wild" singular and plural. right now we only support plural: '*' vs '?'
 * TODO: also for UNIX, support character groupings and negation
 * NOTE: should 'WILD' things eventually be known as 'GLOB' things???
 * http://en.wikipedia.org/wiki/Glob_(programming)
 */
public enum PathnameComponentType {

	/**
	 * Unspecific pathname component type.
	 */
	UNSPECIFIC(":UNSPECIFIC"),

	/**
	 * Wild pathname component type.
	 */
	WILD(":WILD"),

	/**
	 * Nil pathname component type.
	 */
	NIL("NIL");

	/**
	 * String value of the pathname component type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the component type
	 */
	PathnameComponentType(final String value) {
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
