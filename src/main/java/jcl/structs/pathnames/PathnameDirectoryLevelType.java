/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryLevelType} is the enumeration of the directory level type of a 'directory' level
 * element of a Lisp 'pathname' type.
 * NOTE: This implementation does NOT support WildInferiors. Period.
 */
public enum PathnameDirectoryLevelType {

	/**
	 * Wild pathname directory level type.
	 */
	WILD(":WILD"),

	/**
	 * Back pathname directory level type.
	 */
	BACK(":BACK"),

	/**
	 * Up pathname directory level type.
	 */
	UP(":UP"),

	/**
	 * Null pathname directory level type.
	 */
	NULL(null);

	/**
	 * String value of the pathname directory level type.
	 */
	private final String value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory level type
	 */
	PathnameDirectoryLevelType(final String value) {
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
