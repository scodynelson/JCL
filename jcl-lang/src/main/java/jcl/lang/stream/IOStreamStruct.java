/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

/**
 * The {@link IOStreamStruct} is the representation for all Lisp 'stream' types that are both input 'streams' and output
 * 'streams'.
 */
public interface IOStreamStruct extends InputStreamStruct, OutputStreamStruct {

	@Override
	default boolean isInputStream() {
		return true;
	}

	@Override
	default boolean isOutputStream() {
		return true;
	}
}
