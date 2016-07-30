/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class BarStructureObject extends StructureObjectStruct {

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, SymbolStruct.valueOf("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStruct.valueOf("C"), null);
		slots.put(SymbolStruct.valueOf("D"), null);
		slots.put(SymbolStruct.valueOf("E"), null);
	}
}
