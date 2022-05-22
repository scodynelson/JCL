/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import jcl.compiler.environment.binding.lambdalist.WholeParameter;

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
