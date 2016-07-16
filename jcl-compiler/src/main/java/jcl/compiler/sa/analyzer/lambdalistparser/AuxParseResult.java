/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.lang.LispStruct;

final class AuxParseResult extends ParseResult {

	private final List<AuxParameter> auxBindings;

	AuxParseResult(final LispStruct currentElement, final List<AuxParameter> auxBindings) {
		super(currentElement);
		this.auxBindings = auxBindings;
	}

	List<AuxParameter> getAuxBindings() {
		return auxBindings;
	}
}
