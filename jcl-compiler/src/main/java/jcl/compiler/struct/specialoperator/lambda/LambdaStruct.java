/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.lambda;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.array.StringStructImpl;

public class LambdaStruct extends CompilerSpecialOperatorStruct {

	private final String className;

	private final OrdinaryLambdaList lambdaListBindings;

	private final StringStructImpl docString;

	private final PrognStruct forms;

	private final Environment lambdaEnvironment;

	public LambdaStruct(final String className, final OrdinaryLambdaList lambdaListBindings, final StringStructImpl docString,
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
