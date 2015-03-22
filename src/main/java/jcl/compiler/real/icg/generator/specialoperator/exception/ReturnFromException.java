/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.exception;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SymbolStruct;

public class ReturnFromException extends ProgramErrorException {

	private static final long serialVersionUID = -3468129416293653718L;

	private final SymbolStruct<?> name;

	private final LispStruct result;

	public ReturnFromException(final SymbolStruct<?> name, final LispStruct result) {
		super("Name: " + name + " : Result: " + result);
		this.name = name;
		this.result = result;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public LispStruct getResult() {
		return result;
	}
}
