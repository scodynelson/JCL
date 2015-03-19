/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.exception;

import jcl.LispStruct;

public class ThrowException extends RuntimeException {

	private static final long serialVersionUID = -6556966147073549239L;

	private final LispStruct catchTag;

	private final LispStruct resultForm;

	public ThrowException(final LispStruct catchTag, final LispStruct resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public LispStruct getResultForm() {
		return resultForm;
	}
}
