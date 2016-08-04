/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;

public class ReturnFromStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStructImpl name;

	private final LispStruct result;

	public ReturnFromStruct(final SymbolStructImpl name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}

	public SymbolStructImpl getName() {
		return name;
	}

	public LispStruct getResult() {
		return result;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(RETURN-FROM ");

		final String namePrinted = name.toString();
		builder.append(namePrinted);

		builder.append(' ');

		final String formsPrinted = result.toString();
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}
}
