package jcl.compiler.old.functions;

import jcl.LispStruct;

public class MacroExpandReturn {

	private final LispStruct expandedForm;
	private final boolean wasExpanded;

	public MacroExpandReturn(final LispStruct expandedForm, final boolean wasExpanded) {
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
