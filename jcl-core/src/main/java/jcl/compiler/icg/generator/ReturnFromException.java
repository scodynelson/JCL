/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.io.Serial;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import lombok.Getter;

/**
 * Exception used to facilitate the block/return-from lexical control-flow transfer-of-control (ToC) statements.
 */
@Getter
public class ReturnFromException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	@Serial
	private static final long serialVersionUID = 1L;

	/**
	 * The {@link SymbolStruct} name of the 'block' generated that matches this exception.
	 */
	private final SymbolStruct name;

	/**
	 * The resulting {@link LispStruct} value to return when the appropriate 'block' is hit.
	 */
	private final LispStruct result;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the {@link SymbolStruct} name of the 'block' generated
	 * @param result
	 * 		the resulting {@link LispStruct} value to return when the appropriate 'block' is hit
	 */
	public ReturnFromException(final SymbolStruct name, final LispStruct result) {
		super("Name: " + name + " : Result: " + result);
		this.name = name;
		this.result = result;
	}
}
