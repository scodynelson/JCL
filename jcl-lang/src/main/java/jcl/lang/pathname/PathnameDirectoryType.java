/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname'
 * type.
 */
public enum PathnameDirectoryType {

	/**
	 * Absolute pathname directory type.
	 */
	ABSOLUTE(CommonLispSymbols.ABSOLUTE_KEYWORD),

	/**
	 * Relative pathname directory type.
	 */
	RELATIVE(CommonLispSymbols.RELATIVE_KEYWORD);

	/**
	 * Value of the pathname directory type.
	 */
	private final LispStruct value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory type
	 */
	PathnameDirectoryType(final LispStruct value) {
		this.value = value;
	}

	/**
	 * Getter for {@link #value} property.
	 *
	 * @return {@link #value} property
	 */
	public LispStruct getValue() {
		return value;
	}

	/**
	 * Gets the matching PathnameDirectoryType from the provided {@code value} or {@code null} if no match was found.
	 *
	 * @param value
	 * 		the value to find a matching PathnameDirectoryType
	 *
	 * @return the matching PathnameDirectoryType or {@code null} if no match was found
	 */
	public static PathnameDirectoryType fromValue(final LispStruct value) {
		for (final PathnameDirectoryType directoryType : values()) {
			if (directoryType.value.equals(value)) {
				return directoryType;
			}
		}
		return null;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(value)
		                                                                .toString();
	}
}
