/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.numbers.RatioStruct;

public class RatioElement implements Element {

	private static final long serialVersionUID = 5051668665204048945L;

	private final RatioStruct ratioStruct;

	public RatioElement(final RatioStruct ratioStruct) {
		this.ratioStruct = ratioStruct;
	}

	public RatioStruct getRatioStruct() {
		return ratioStruct;
	}
}
