/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.struct;

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
