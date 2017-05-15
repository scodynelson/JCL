/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.LispStruct;

public class MacroExpandResult {

	private final LispStruct expandedForm;

	private final boolean wasExpanded;

	public MacroExpandResult(final LispStruct expandedForm, final boolean wasExpanded) {
		this.expandedForm = expandedForm;
		this.wasExpanded = wasExpanded;
	}

	public LispStruct getExpandedForm() {
		return expandedForm;
	}

	public boolean wasExpanded() {
		return wasExpanded;
	}
}
