/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.readtable;

/**
 * Defines the different attributes types that will determine how the reader parses tokens.
 */
public enum AttributeType {

	/**
	 * Invalid attribute.
	 */
	INVALID,

	/**
	 * Alphabetic attribute.
	 */
	ALPHABETIC,

	/**
	 * Plus sign attribute.
	 */
	PLUS,

	/**
	 * Minus sign attribute.
	 */
	MINUS,

	/**
	 * Decimal point attribute.
	 */
	DECIMAL,

	/**
	 * Ratio marker attribute.
	 */
	RATIOMARKER,

	/**
	 * Alpha-numeric attribute.
	 */
	ALPHADIGIT,

	/**
	 * Package marker attribute.
	 */
	PACKAGEMARKER,

	/**
	 * Exponent marker attribute.
	 */
	EXPONENTMARKER
}
