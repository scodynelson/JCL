/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class UnwindProtectStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct protectedForm;

	private final PrognStruct cleanupForms;

	public UnwindProtectStruct(final LispStruct protectedForm, final PrognStruct cleanupForms) {
		this.protectedForm = protectedForm;
		this.cleanupForms = cleanupForms;
	}

	public LispStruct getProtectedForm() {
		return protectedForm;
	}

	public PrognStruct getCleanupForms() {
		return cleanupForms;
	}
}
