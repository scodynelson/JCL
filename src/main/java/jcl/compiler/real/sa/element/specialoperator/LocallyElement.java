/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class LocallyElement implements Element {

	private static final long serialVersionUID = 3549306656634788482L;

	private final List<Element> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public LocallyElement(final List<Element> forms, final LexicalEnvironment lexicalEnvironment) {
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}
}
