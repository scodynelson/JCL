/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.EnvironmentParameter;

final class EnvironmentParseResult extends ParseResult {

	private final EnvironmentParameter environmentBinding;

	EnvironmentParseResult(final LispStruct currentElement, final EnvironmentParameter environmentBinding) {
		super(currentElement);
		this.environmentBinding = environmentBinding;
	}

	EnvironmentParameter getEnvironmentBinding() {
		return environmentBinding;
	}
}
