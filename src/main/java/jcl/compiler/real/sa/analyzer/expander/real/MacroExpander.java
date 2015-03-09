/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<E extends LispStruct> extends FunctionStruct {

	private static final long serialVersionUID = 4976821260573562104L;

	@Override
	public LispStruct apply(LispStruct... lispStructs) {
		// TODO: do this
		return null;
	}

	public abstract LispStruct expand(E form, AnalysisBuilder analysisBuilder);
}
