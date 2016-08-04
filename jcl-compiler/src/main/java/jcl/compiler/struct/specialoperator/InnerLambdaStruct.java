/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.SymbolStruct;

public class InnerLambdaStruct extends CompilerSpecialOperatorStruct {

	private final List<InnerLambdaVar> vars;

	private final PrognStruct forms;

	private final Environment lexicalEnvironment;

	public InnerLambdaStruct(final List<InnerLambdaVar> vars, final PrognStruct forms, final Environment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<InnerLambdaVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class InnerLambdaVar {

		private final SymbolStruct var;

		private final CompilerFunctionStruct initForm;

		private final boolean isSpecial;

		public InnerLambdaVar(final SymbolStruct var, final CompilerFunctionStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public CompilerFunctionStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}
	}
}
