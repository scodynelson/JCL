/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import jcl.LispStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryLevelType} is the enumeration of the directory level type of a 'directory' level
 * element of a Lisp 'pathname' type.
 */
public enum PathnameDirectoryLevelType {

	/**
	 * Wild pathname directory level type.
	 */
	WILD(CommonLispSymbols.WILD),

	/**
	 * Wild-Inferiors pathname directory level type.
	 */
	WILD_INFERIORS(CommonLispSymbols.WILD_INFERIORS),

	/**
	 * Back pathname directory level type.
	 */
	BACK(CommonLispSymbols.BACK),

	/**
	 * Up pathname directory level type.
	 */
	UP(CommonLispSymbols.UP),

	/**
	 * Null pathname directory level type.
	 */
	NULL(CommonLispSymbols.NIL);

	/**
	 * Value of the pathname directory level type.
	 */
	private final LispStruct value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the directory level type
	 */
	PathnameDirectoryLevelType(final LispStruct value) {
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
	 * Gets the matching PathnameDirectoryLevelType from the provided {@code value} or {@code null} if no match was
	 * found.
	 *
	 * @param value
	 * 		the value to find a matching PathnameDirectoryLevelType
	 *
	 * @return the matching PathnameDirectoryLevelType or {@code null} if no match was found
	 */
	public static PathnameDirectoryLevelType fromValue(final LispStruct value) {
		for (final PathnameDirectoryLevelType directoryLevelType : values()) {
			if (directoryLevelType.value.equals(value)) {
				return directoryLevelType;
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
