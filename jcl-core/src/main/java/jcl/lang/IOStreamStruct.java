/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link IOStreamStruct} is the representation for all Lisp 'stream' types that are both input 'streams' and output
 * 'streams'.
 */
public interface IOStreamStruct extends InputStreamStruct, OutputStreamStruct {

	/*
	STREAM-STRUCT
	 */

	@Override
	default BooleanStruct inputStreamP() {
		return TStruct.INSTANCE;
	}

	@Override
	default BooleanStruct outputStreamP() {
		return TStruct.INSTANCE;
	}
}
