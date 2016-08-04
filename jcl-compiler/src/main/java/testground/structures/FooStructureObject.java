/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStructImpl;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStruct {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, SymbolStructImpl.valueOf("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStructImpl.valueOf("A"), null);
		slots.put(SymbolStructImpl.valueOf("B"), null);
		slots.put(SymbolStructImpl.valueOf("C"), null);
	}
}
