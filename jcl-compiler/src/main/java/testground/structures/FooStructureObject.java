/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStruct {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, SymbolStruct.valueOf("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStruct.valueOf("A"), null);
		slots.put(SymbolStruct.valueOf("B"), null);
		slots.put(SymbolStruct.valueOf("C"), null);
	}
}
