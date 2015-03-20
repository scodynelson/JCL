/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.exception;

import jcl.LispStruct;

public class GoException extends RuntimeException {

	private static final long serialVersionUID = 4353009450417754927L;

	private final LispStruct tag;

	public GoException(final LispStruct tag) {
		this.tag = tag;
	}

	public LispStruct getTag() {
		return tag;
	}
}
