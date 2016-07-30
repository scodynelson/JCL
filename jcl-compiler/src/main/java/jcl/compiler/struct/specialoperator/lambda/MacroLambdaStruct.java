/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.lambda;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.array.StringStructImpl;

public class MacroLambdaStruct extends CompilerSpecialOperatorStruct {

	private final String className;

	private final SymbolStruct macroName;

	private final MacroLambdaList lambdaListBindings;

	private final StringStructImpl docString;

	private final PrognStruct forms;

	private final Environment lambdaEnvironment;

	public MacroLambdaStruct(final String className, final SymbolStruct macroName, final MacroLambdaList lambdaListBindings,
	                         final StringStructImpl docString, final PrognStruct forms, final Environment lambdaEnvironment) {
		this.className = className;
		this.macroName = macroName;
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public String getClassName() {
		return className;
	}

	public SymbolStruct getMacroName() {
		return macroName;
	}

	public MacroLambdaList getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStructImpl getDocString() {
		return docString;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLambdaEnvironment() {
		return lambdaEnvironment;
	}
}
