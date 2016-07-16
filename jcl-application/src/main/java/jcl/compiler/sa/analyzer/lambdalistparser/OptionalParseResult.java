/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.lang.LispStruct;

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
