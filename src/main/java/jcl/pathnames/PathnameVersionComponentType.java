/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import jcl.LispStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameVersionComponentType} is the enumeration of the type of a component element of the version
 * element of a Lisp 'pathname' type.
 */
public enum PathnameVersionComponentType {

	/**
	 * Unspecific pathname version component type.
	 */
	UNSPECIFIC(CommonLispSymbols.UNSPECIFIC_KEYWORD),

	/**
	 * Wild pathname version component type.
	 */
	WILD(CommonLispSymbols.WILD_KEYWORD),

	/**
	 * Nil pathname version component type.
	 */
	NIL(CommonLispSymbols.NIL),

	/**
	 * Newest pathname version component type.
	 */
	NEWEST(CommonLispSymbols.NEWEST_KEYWORD),

	/**
	 * Oldest pathname version component type.
	 */
	OLDEST(CommonLispSymbols.OLDEST_KEYWORD);

	/**
	 * Value of the pathname version component type.
	 */
	private final LispStruct value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the version component type
	 */
	PathnameVersionComponentType(final LispStruct value) {
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
	 * Gets the matching PathnameVersionComponentType from the provided {@code value} or {@code null} if no match was
	 * found.
	 *
	 * @param value
	 * 		the value to find a matching PathnameVersionComponentType
	 *
	 * @return the matching PathnameVersionComponentType or {@code null} if no match was found
	 */
	public static PathnameVersionComponentType fromValue(final LispStruct value) {
		for (final PathnameVersionComponentType versionComponentType : values()) {
			if (versionComponentType.value.equals(value)) {
				return versionComponentType;
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
