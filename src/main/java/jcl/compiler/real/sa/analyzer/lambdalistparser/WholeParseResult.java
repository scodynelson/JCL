/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.compiler.real.environment.binding.lambdalist.WholeParameter;

final class WholeParseResult extends ParseResult {

	private final WholeParameter wholeBinding;

	WholeParseResult(final WholeParameter wholeBinding) {
		super(null);
		this.wholeBinding = wholeBinding;
	}

	WholeParameter getWholeBinding() {
		return wholeBinding;
	}
}
