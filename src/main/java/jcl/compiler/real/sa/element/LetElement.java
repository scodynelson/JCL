/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class LetElement implements Element {

	private static final long serialVersionUID = -3186671381163635893L;

	private final List<LetVar> vars;
	private final List<LispStruct> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public LetElement(final List<LetVar> vars, final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public static class LetVar {

		private SymbolStruct<?> var;
		private LispStruct initForm;

		public LetVar(final SymbolStruct<?> var, final LispStruct initForm) {
			this.var = var;
			this.initForm = initForm;
		}
	}
}
