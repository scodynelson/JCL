/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import jcl.lang.LispStruct;

class ParseResult {

	private final LispStruct currentElement;

	ParseResult(final LispStruct currentElement) {
		this.currentElement = currentElement;
	}

	LispStruct getCurrentElement() {
		return currentElement;
	}
}
