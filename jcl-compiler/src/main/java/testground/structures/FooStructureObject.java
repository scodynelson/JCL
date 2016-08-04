/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.factory.LispStructFactory;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStruct {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, LispStructFactory.toSymbol("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(LispStructFactory.toSymbol("A"), null);
		slots.put(LispStructFactory.toSymbol("B"), null);
		slots.put(LispStructFactory.toSymbol("C"), null);
	}
}
