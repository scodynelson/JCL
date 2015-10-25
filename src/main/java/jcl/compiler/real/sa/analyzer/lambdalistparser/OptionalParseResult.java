/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalParameter;

final class OptionalParseResult extends ParseResult {

	private final List<OptionalParameter> optionalBindings;

	OptionalParseResult(final LispStruct currentElement, final List<OptionalParameter> optionalBindings) {
		super(currentElement);
		this.optionalBindings = optionalBindings;
	}

	List<OptionalParameter> getOptionalBindings() {
		return optionalBindings;
	}
}
