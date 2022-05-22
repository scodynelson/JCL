/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.io.Serial;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import lombok.Getter;

/**
 * Exception used to facilitate the catch/throw dynamic control-flow transfer-of-control (ToC) statements.
 */
@Getter
public class ThrowException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	@Serial
	private static final long serialVersionUID = 1L;

	/**
	 * The {@link LispStruct} tag of the 'catch' generated that matches this exception.
	 */
	@SuppressWarnings("TransientFieldNotInitialized")
	private final transient LispStruct catchTag;

	/**
	 * The resulting {@link LispStruct} value to return when the appropriate 'catch' is hit.
	 */
	@SuppressWarnings("TransientFieldNotInitialized")
	private final transient LispStruct resultForm;

	/**
	 * Public constructor.
	 *
	 * @param catchTag
	 * 		the {@link LispStruct} tag of the 'catch' generated
	 * @param resultForm
	 * 		the resulting {@link LispStruct} value to return when the appropriate 'catch' is hit
	 */
	public ThrowException(final LispStruct catchTag, final LispStruct resultForm) {
		super("Tag: " + catchTag + " : Result: " + resultForm);
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}
}
