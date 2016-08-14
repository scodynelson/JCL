/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.stream.ReadPeekResult;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
public interface Reader {

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStreamStruct}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link InputStreamStruct}
	 */
	LispStruct read(ReaderInputStreamStruct inputStreamStruct, boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStreamStruct}, making sure to preserve any whitespace
	 * characters after the {@link LispStruct} token is read.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link InputStreamStruct}
	 */
	LispStruct readPreservingWhitespace(ReaderInputStreamStruct inputStreamStruct, boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link InputStreamStruct}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link ReadPeekResult} from the {@link InputStreamStruct}
	 */
	ReadPeekResult readChar(ReaderInputStreamStruct inputStreamStruct, boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Un-reads the provided {@code codePoint} value from (or really back into) the {@link InputStreamStruct}.
	 *
	 * @param codePoint
	 * 		the value to un-read from (or really back into) the {@link InputStreamStruct}
	 */
	void unreadChar(ReaderInputStreamStruct inputStreamStruct, int codePoint);
}
