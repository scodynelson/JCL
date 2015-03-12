/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<O extends LispStruct, I extends LispStruct> extends FunctionStruct {

	private static final long serialVersionUID = 4976821260573562104L;

	@Override
	public LispStruct apply(LispStruct... lispStructs) {
		// TODO: do this
		return null;
	}

	public abstract O expand(I form, AnalysisBuilder analysisBuilder);
}
