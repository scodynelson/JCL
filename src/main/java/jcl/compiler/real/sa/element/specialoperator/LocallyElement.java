/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class LocallyElement implements Element {

	private static final long serialVersionUID = 3549306656634788482L;

	private final List<LispStruct> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public LocallyElement(final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}
}
