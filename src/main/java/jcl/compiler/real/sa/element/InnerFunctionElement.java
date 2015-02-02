/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public abstract class InnerFunctionElement implements Element {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<? extends InnerFunctionVar> vars;
	private final List<LispStruct> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public InnerFunctionElement(final List<? extends InnerFunctionVar> vars, final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public abstract static class InnerFunctionVar {

		private SymbolStruct<?> var;
		private LispStruct initForm;

		public InnerFunctionVar(final SymbolStruct<?> var, final LispStruct initForm) {
			this.var = var;
			this.initForm = initForm;
		}
	}
}
