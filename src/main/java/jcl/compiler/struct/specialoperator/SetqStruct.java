/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class SetqStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -5092653942359022766L;

	private final List<SetqPair> setqPairs;

	public SetqStruct(final List<SetqPair> setqPairs) {
		this.setqPairs = setqPairs;
	}

	public List<SetqPair> getSetqPairs() {
		return setqPairs;
	}

	public static class SetqPair implements Serializable {

		private static final long serialVersionUID = -7804939280136663517L;

		private final SymbolStruct<?> var;

		private final LispStruct form;

		public SetqPair(final SymbolStruct<?> var, final LispStruct form) {
			this.var = var;
			this.form = form;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getForm() {
			return form;
		}
	}
}