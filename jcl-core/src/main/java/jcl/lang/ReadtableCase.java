/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * Defines the case types that will be used when reading tokens.
 */
public enum ReadtableCase {

	/**
	 * Uppercase spec.
	 */
	UPCASE,

	/**
	 * Lowercase spec.
	 */
	DOWNCASE,

	/**
	 * Invert case spec.
	 */
	INVERT,

	/**
	 * Preserve case spec.
	 */
	PRESERVE
}
