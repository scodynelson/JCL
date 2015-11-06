/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.lambda;

import jcl.arrays.StringStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;

public class LambdaStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -3234593852889478530L;

	private final String className;

	private final OrdinaryLambdaList lambdaListBindings;

	private final StringStruct docString;

	private final PrognStruct forms;

	private final Environment lambdaEnvironment;

	public LambdaStruct(final String className, final OrdinaryLambdaList lambdaListBindings, final StringStruct docString,
	                    final PrognStruct forms, final Environment lambdaEnvironment) {
		this.className = className;
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public String getClassName() {
		return className;
	}

	public OrdinaryLambdaList getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLambdaEnvironment() {
		return lambdaEnvironment;
	}
}