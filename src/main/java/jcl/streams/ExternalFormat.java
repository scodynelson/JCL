/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import java.nio.charset.Charset;

import jcl.LispStruct;

public enum ExternalFormat implements LispStruct {

	DEFAULT(Charset.defaultCharset());

	private final Charset charset;

	ExternalFormat(final Charset charset) {
		this.charset = charset;
	}

	public Charset getCharset() {
		return charset;
	}
}
