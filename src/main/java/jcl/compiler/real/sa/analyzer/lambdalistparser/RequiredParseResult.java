/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;

final class RequiredParseResult extends ParseResult {

	private final List<RequiredParameter> requiredBindings;

	RequiredParseResult(final LispStruct currentElement, final List<RequiredParameter> requiredBindings) {
		super(currentElement);
		this.requiredBindings = requiredBindings;
	}

	List<RequiredParameter> getRequiredBindings() {
		return requiredBindings;
	}
}
