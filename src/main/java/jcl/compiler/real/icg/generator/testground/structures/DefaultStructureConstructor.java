/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import jcl.LispStruct;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;

public class DefaultStructureConstructor extends FunctionStruct {

	private static final long serialVersionUID = -6252440259035380617L;

	public DefaultStructureConstructor(final Closure parentClosure) {
		closure = parentClosure;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
