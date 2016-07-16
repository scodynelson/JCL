/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.lang.LispStruct;

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
