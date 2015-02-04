/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.lists.NullStruct;

public final class NullElement implements Element {

	public static final NullElement INSTANCE = new NullElement();

	private static final long serialVersionUID = -5896411725518882129L;

	private final NullStruct nullStruct;

	private NullElement() {
		nullStruct = NullStruct.INSTANCE;
	}

	public NullStruct getNullStruct() {
		return nullStruct;
	}
}
