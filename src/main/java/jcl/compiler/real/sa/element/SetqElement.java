/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class SetqElement implements Element {

	private static final long serialVersionUID = -5092653942359022766L;

	private final List<SetqPair> setqPairs;

	public SetqElement(final List<SetqPair> setqPairs) {
		this.setqPairs = setqPairs;
	}

	public static class SetqPair {

		private final SymbolStruct<?> var;
		private final LispStruct form;

		public SetqPair(final SymbolStruct<?> var, final LispStruct form) {
			this.var = var;
			this.form = form;
		}
	}
}
