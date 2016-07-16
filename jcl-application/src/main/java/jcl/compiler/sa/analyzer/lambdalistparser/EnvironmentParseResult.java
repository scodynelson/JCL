/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;

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
