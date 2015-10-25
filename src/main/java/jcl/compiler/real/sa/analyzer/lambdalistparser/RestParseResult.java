/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.RestParameter;

final class RestParseResult extends ParseResult {

	private final RestParameter restBinding;

	RestParseResult(final LispStruct currentElement, final RestParameter restBinding) {
		super(currentElement);
		this.restBinding = restBinding;
	}

	RestParameter getRestBinding() {
		return restBinding;
	}
}
