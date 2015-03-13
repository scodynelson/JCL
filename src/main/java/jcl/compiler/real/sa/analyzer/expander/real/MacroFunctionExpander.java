/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.MacroExpander;
import jcl.lists.ListStruct;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	private static final long serialVersionUID = -4041262906159677088L;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
