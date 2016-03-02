/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

/**
 * Created by codynelson on 2/7/16.
 */
public class ReadLineResult {

	private final String result;

	private final boolean eof;

	ReadLineResult(final String result, final boolean wasEof) {
		this.result = result;
		eof = wasEof;
	}

	public String getResult() {
		return result;
	}

	public boolean isEof() {
		return eof;
	}
}
