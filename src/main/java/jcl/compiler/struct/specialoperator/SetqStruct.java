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
