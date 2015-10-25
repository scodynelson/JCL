/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.KeyParameter;

final class KeyParseResult extends ParseResult {

	private final List<KeyParameter> keyBindings;

	KeyParseResult(final LispStruct currentElement, final List<KeyParameter> keyBindings) {
		super(currentElement);
		this.keyBindings = keyBindings;
	}

	List<KeyParameter> getKeyBindings() {
		return keyBindings;
	}
}
