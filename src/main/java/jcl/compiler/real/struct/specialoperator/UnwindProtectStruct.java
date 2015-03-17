/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class UnwindProtectStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 2849602976511423223L;

	private final LispStruct protectedForm;

	private final List<LispStruct> cleanupForms;

	public UnwindProtectStruct(final LispStruct protectedForm, final List<LispStruct> cleanupForms) {
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
