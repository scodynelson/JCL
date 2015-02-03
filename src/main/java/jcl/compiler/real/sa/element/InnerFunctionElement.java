/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.symbols.SymbolStruct;

import java.io.Serializable;
import java.util.List;

public abstract class InnerFunctionElement implements Element {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<? extends InnerFunctionVar> vars;
	private final List<LispStruct> forms;

	private final LexicalEnvironment lexicalEnvironment;

	InnerFunctionElement(final List<? extends InnerFunctionVar> vars, final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<? extends InnerFunctionVar> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public abstract static class InnerFunctionVar implements Serializable {

		private static final long serialVersionUID = 891453745075246590L;

		private final SymbolStruct<?> var;
		private final LispStruct initForm;

		InnerFunctionVar(final SymbolStruct<?> var, final LispStruct initForm) {
			this.var = var;
			this.initForm = initForm;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getInitForm() {
			return initForm;
		}
	}
}
