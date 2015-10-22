/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;

final class OptionalParseResult extends ParseResult {

	private final List<OptionalBinding> optionalBindings;

	OptionalParseResult(final LispStruct currentElement, final List<OptionalBinding> optionalBindings) {
		super(currentElement);
		this.optionalBindings = optionalBindings;
	}

	List<OptionalBinding> getOptionalBindings() {
		return optionalBindings;
	}
}
