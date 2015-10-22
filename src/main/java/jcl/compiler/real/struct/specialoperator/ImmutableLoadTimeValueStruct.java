/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class ImmutableLoadTimeValueStruct extends CompilerSpecialOperatorStruct implements LoadTimeValueStruct {

	private static final long serialVersionUID = 857211495712280441L;

	private final String uniqueLTVId;

	public ImmutableLoadTimeValueStruct(final String uniqueLTVId) {
		this.uniqueLTVId = uniqueLTVId;
	}

	public String getUniqueLTVId() {
		return uniqueLTVId;
	}
}
