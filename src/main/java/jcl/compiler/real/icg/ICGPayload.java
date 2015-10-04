/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import jcl.LispStruct;

public class ICGPayload {

	private final LispStruct payload;

	private final GeneratorState generatorState;

	public ICGPayload(final LispStruct payload, final GeneratorState generatorState) {
		this.payload = payload;
		this.generatorState = generatorState;
	}

	public LispStruct getPayload() {
		return payload;
	}

	public GeneratorState getGeneratorState() {
		return generatorState;
	}
}
