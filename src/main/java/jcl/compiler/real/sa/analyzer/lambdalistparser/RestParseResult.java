/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;

final class RestParseResult extends ParseResult {

	private final RestBinding restBinding;

	RestParseResult(final LispStruct currentElement, final RestBinding restBinding) {
		super(currentElement);
		this.restBinding = restBinding;
	}

	RestBinding getRestBinding() {
		return restBinding;
	}
}
