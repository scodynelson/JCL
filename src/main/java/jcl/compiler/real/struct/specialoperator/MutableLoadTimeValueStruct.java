/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class MutableLoadTimeValueStruct extends CompilerSpecialOperatorStruct implements LoadTimeValueStruct {

	private static final long serialVersionUID = 8088799347738800471L;

	private final LispStruct form;

	public MutableLoadTimeValueStruct(final LispStruct form) {
		this.form = form;
	}

	public LispStruct getForm() {
		return form;
	}
}
