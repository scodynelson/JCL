/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.NILStruct;
import jcl.lang.internal.StructureObjectStructImpl;
import jcl.lang.factory.LispStructFactory;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStructImpl {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, LispStructFactory.toSymbol("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(LispStructFactory.toSymbol("A"), NILStruct.INSTANCE);
		slots.put(LispStructFactory.toSymbol("B"), NILStruct.INSTANCE);
		slots.put(LispStructFactory.toSymbol("C"), NILStruct.INSTANCE);
	}
}
