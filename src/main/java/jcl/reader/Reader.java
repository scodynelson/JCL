/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
public interface Reader {

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStream}. This calls the overloaded {@link #read(boolean,
	 * LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and {@code recursiveP} as
	 * true.
	 *
	 * @return the next {@link LispStruct} from the {@link InputStream}.
	 */
	LispStruct read();

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link InputStream}.
	 */
	LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link InputStream}. This calls the overloaded {@link
	 * #readChar(boolean, LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and
	 * {@code recursiveP} as true.
	 *
	 * @return the next {@link ReadPeekResult} from the {@link InputStream}.
	 */
	ReadPeekResult readChar();

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link InputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link ReadPeekResult} from the {@link InputStream}.
	 */
	ReadPeekResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Un-reads the provided {@code codePoint} value from (or really back into) the {@link InputStream}.
	 *
	 * @param codePoint
	 * 		the value to un-read from (or really back into) the {@link InputStream}
	 */
	void unreadChar(int codePoint);

	/**
	 * Gets the {@link InputStream} for the JCL Reader instance.
	 *
	 * @return the {@link InputStream} for the JCL Reader instance.
	 */
	InputStream getInputStream();
}
