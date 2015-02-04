/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class LambdaElement implements Element {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final List<Element> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public LambdaElement(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final List<Element> forms,
	                     final LexicalEnvironment lexicalEnvironment) {
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}
}
