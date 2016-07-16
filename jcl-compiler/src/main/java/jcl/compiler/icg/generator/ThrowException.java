/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ProgramErrorException;

/**
 * Exception used to facilitate the catch/throw dynamic control-flow transfer-of-control (ToC) statements.
 */
public class ThrowException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6556966147073549239L;

	/**
	 * The {@link LispStruct} tag of the 'catch' generated via the {@link CatchCodeGenerator} that matches this
	 * exception.
	 */
	private final LispStruct catchTag;

	/**
	 * The resulting {@link LispStruct} value to return when the appropriate 'catch' is hit.
	 */
	private final LispStruct resultForm;

	/**
	 * Public constructor.
	 *
	 * @param catchTag
	 * 		the {@link LispStruct} tag of the 'catch' generated via the {@link CatchCodeGenerator}
	 * @param resultForm
	 * 		the resulting {@link LispStruct} value to return when the appropriate 'catch' is hit
	 */
	public ThrowException(final LispStruct catchTag, final LispStruct resultForm) {
		super("Tag: " + catchTag + " : Result: " + resultForm);
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	/**
	 * Getter for the values of the {@link #catchTag} property.
	 *
	 * @return the value of the {@link #catchTag} property
	 */
	public LispStruct getCatchTag() {
		return catchTag;
	}

	/**
	 * Getter for the values of the {@link #resultForm} property.
	 *
	 * @return the value of the {@link #resultForm} property
	 */
	public LispStruct getResultForm() {
		return resultForm;
	}
}
