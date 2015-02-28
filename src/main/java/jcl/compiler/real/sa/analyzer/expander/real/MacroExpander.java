/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<E extends SimpleElement> extends FunctionStruct {

	private static final long serialVersionUID = 4976821260573562104L;

	@Override
	public LispStruct apply(LispStruct... lispStructs) {
		// TODO: do this
		return null;
	}

	public abstract Element expand(E form, AnalysisBuilder analysisBuilder);
}
