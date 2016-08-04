/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;

/**
 * Exception used to facilitate the block/return-from lexical control-flow transfer-of-control (ToC) statements.
 */
public class ReturnFromException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3468129416293653718L;

	/**
	 * The {@link SymbolStructImpl} name of the 'block' generated via the {@link BlockCodeGenerator} that matches this
	 * exception.
	 */
	private final SymbolStructImpl name;

	/**
	 * The resulting {@link LispStruct} value to return when the appropriate 'block' is hit.
	 */
	private final LispStruct result;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the {@link SymbolStructImpl} name of the 'block' generated via the {@link BlockCodeGenerator}
	 * @param result
	 * 		the resulting {@link LispStruct} value to return when the appropriate 'block' is hit
	 */
	public ReturnFromException(final SymbolStructImpl name, final LispStruct result) {
		super("Name: " + name + " : Result: " + result);
		this.name = name;
		this.result = result;
	}

	/**
	 * Getter for the values of the {@link #name} property.
	 *
	 * @return the value of the {@link #name} property
	 */
	public SymbolStructImpl getName() {
		return name;
	}

	/**
	 * Getter for the values of the {@link #result} property.
	 *
	 * @return the value of the {@link #result} property
	 */
	public LispStruct getResult() {
		return result;
	}
}
