/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.lambda;

import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;

public class MacroLambdaStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 8461845259414025247L;

	private final String fileName;

	private final SymbolStruct<?> macroName;

	private final MacroLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final PrognStruct forms;

	private final LambdaEnvironment lambdaEnvironment;

	public MacroLambdaStruct(final String fileName, final SymbolStruct<?> macroName, final MacroLambdaListBindings lambdaListBindings,
	                         final StringStruct docString, final PrognStruct forms, final LambdaEnvironment lambdaEnvironment) {
		this.fileName = fileName;
		this.macroName = macroName;
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public String getFileName() {
		return fileName;
	}

	public MacroLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}
}
