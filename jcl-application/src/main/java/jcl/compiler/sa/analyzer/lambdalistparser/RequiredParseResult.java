/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lang.LispStruct;

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
