/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

/**
 * The {@link IOStream} is the representation for all Lisp 'stream' types that are both input 'streams' and output
 * 'streams'.
 */
public interface IOStream extends InputStream, OutputStream {

	@Override
	default boolean isInputStream() {
		return true;
	}

	@Override
	default boolean isOutputStream() {
		return true;
	}
}
