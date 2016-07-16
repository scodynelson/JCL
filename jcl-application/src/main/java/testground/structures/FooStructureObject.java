/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStruct {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, new SymbolStruct("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(new SymbolStruct("A"), null);
		slots.put(new SymbolStruct("B"), null);
		slots.put(new SymbolStruct("C"), null);
	}
}
