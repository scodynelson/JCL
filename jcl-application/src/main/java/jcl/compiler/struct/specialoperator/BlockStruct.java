/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class BlockStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;

	private final PrognStruct forms;

	public BlockStruct(final SymbolStruct name, final List<LispStruct> forms) {
		this.name = name;
		this.forms = new PrognStruct(forms);
	}

	public SymbolStruct getName() {
		return name;
	}

	public PrognStruct getForms() {
		return forms;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(BLOCK ");

		final String namePrinted = name.toString();
		builder.append(namePrinted);

		builder.append(' ');

		final String formsPrinted = forms.toString();
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}
}
