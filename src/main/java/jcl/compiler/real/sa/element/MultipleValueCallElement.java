/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class MultipleValueCallElement implements Element {

	private static final long serialVersionUID = 2789725049143539321L;

	// TODO: do we do evaluation in the SA to product an actual 'FunctionStruct' object here?
	private final LispStruct functionForm;
	private final List<LispStruct> forms;

	public MultipleValueCallElement(final LispStruct functionForm, final List<LispStruct> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}
}
