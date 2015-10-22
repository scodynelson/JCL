/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.EnvironmentBinding;

final class EnvironmentParseResult extends ParseResult {

	private final EnvironmentBinding environmentBinding;

	EnvironmentParseResult(final LispStruct currentElement, final EnvironmentBinding environmentBinding) {
		super(currentElement);
		this.environmentBinding = environmentBinding;
	}

	EnvironmentBinding getEnvironmentBinding() {
		return environmentBinding;
	}
}
