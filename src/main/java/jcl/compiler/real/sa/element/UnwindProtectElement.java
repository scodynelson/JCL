/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class UnwindProtectElement implements Element {

	private static final long serialVersionUID = 2849602976511423223L;

	private final LispStruct protectedForm;
	private final List<LispStruct> cleanupForms;

	public UnwindProtectElement(final LispStruct protectedForm, final List<LispStruct> cleanupForms) {
		this.protectedForm = protectedForm;
		this.cleanupForms = cleanupForms;
	}

	public LispStruct getProtectedForm() {
		return protectedForm;
	}

	public List<LispStruct> getCleanupForms() {
		return cleanupForms;
	}
}
