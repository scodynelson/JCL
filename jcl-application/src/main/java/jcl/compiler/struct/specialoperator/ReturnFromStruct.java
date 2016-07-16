/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class ReturnFromStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;

	private final LispStruct result;

	public ReturnFromStruct(final SymbolStruct name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}

	public SymbolStruct getName() {
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
