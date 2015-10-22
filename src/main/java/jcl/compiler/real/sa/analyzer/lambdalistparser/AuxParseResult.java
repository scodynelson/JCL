/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;

final class AuxParseResult extends ParseResult {

	private final List<AuxBinding> auxBindings;

	AuxParseResult(final LispStruct currentElement, final List<AuxBinding> auxBindings) {
		super(currentElement);
		this.auxBindings = auxBindings;
	}

	List<AuxBinding> getAuxBindings() {
		return auxBindings;
	}
}
