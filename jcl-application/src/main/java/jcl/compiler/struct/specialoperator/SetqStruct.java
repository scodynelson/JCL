/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class SetqStruct extends CompilerSpecialOperatorStruct {

	private final List<SetqPair> setqPairs;

	public SetqStruct(final List<SetqPair> setqPairs) {
		this.setqPairs = setqPairs;
	}

	public List<SetqPair> getSetqPairs() {
		return setqPairs;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(SETQ");

		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			builder.append(' ');

			final SymbolStruct var = setqPair.getVar();
			final String varPrinted = var.toString();
			builder.append(varPrinted);

			builder.append(' ');

			final LispStruct form = setqPair.getForm();
			final String formPrinted = form.toString();
			builder.append(formPrinted);
		}

		builder.append(')');

		return builder.toString();
	}

	public static class SetqPair {

		private final SymbolStruct var;

		private final LispStruct form;

		public SetqPair(final SymbolStruct var, final LispStruct form) {
			this.var = var;
			this.form = form;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public LispStruct getForm() {
			return form;
		}
	}
}
