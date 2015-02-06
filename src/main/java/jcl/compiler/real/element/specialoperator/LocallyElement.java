/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.LocallyEnvironment;

import java.util.List;

public class LocallyElement implements Element {

	private static final long serialVersionUID = 3549306656634788482L;

	private final List<Element> forms;

	private final LocallyEnvironment locallyEnvironment;

	public LocallyElement(final List<Element> forms, final LocallyEnvironment locallyEnvironment) {
		this.forms = forms;
		this.locallyEnvironment = locallyEnvironment;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LocallyEnvironment getLocallyEnvironment() {
		return locallyEnvironment;
	}
}
