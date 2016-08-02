/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.LispStruct;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameCaseType} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
 */
public enum PathnameCaseType {

	/**
	 * Common pathname case type.
	 */
	COMMON(CommonLispSymbols.COMMON_KEYWORD),

	/**
	 * Local pathname case type.
	 */
	LOCAL(CommonLispSymbols.LOCAL_KEYWORD);

	/**
	 * Value of the pathname case type.
	 */
	private final LispStruct value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the case type
	 */
	PathnameCaseType(final LispStruct value) {
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
	 * Gets the matching PathnameCaseType from the provided {@code value} or {@code null} if no match was found.
	 *
	 * @param value
	 * 		the value to find a matching PathnameCaseType
	 *
	 * @return the matching PathnameCaseType or {@code null} if no match was found
	 */
	public static PathnameCaseType fromValue(final LispStruct value) {
		for (final PathnameCaseType caseType : values()) {
			if (caseType.value.equals(value)) {
				return caseType;
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
