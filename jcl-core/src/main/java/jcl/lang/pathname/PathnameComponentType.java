/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.statics.CommonLispSymbols;

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
	UNSPECIFIC(CommonLispSymbols.UNSPECIFIC_KEYWORD),

	/**
	 * Wild pathname component type.
	 */
	WILD(CommonLispSymbols.WILD_KEYWORD),

	/**
	 * Nil pathname component type.
	 */
	NIL(NILStruct.INSTANCE);

	/**
	 * Value of the pathname component type.
	 */
	private final LispStruct value;

	/**
	 * Constructor.
	 *
	 * @param value
	 * 		value of the component type
	 */
	PathnameComponentType(final LispStruct value) {
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
	 * Gets the matching PathnameComponentType from the provided {@code value} or {@code null} if no match was found.
	 *
	 * @param value
	 * 		the value to find a matching PathnameComponentType
	 *
	 * @return the matching PathnameComponentType or {@code null} if no match was found
	 */
	public static PathnameComponentType fromValue(final LispStruct value) {
		for (final PathnameComponentType componentType : values()) {
			if (componentType.value.eql(value)) {
				return componentType;
			}
		}
		return null;
	}
}
