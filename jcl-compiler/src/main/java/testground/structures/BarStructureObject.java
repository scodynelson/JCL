/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStructImpl;

@SuppressWarnings("all")
public class BarStructureObject extends StructureObjectStruct {

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, SymbolStructImpl.valueOf("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStructImpl.valueOf("C"), null);
		slots.put(SymbolStructImpl.valueOf("D"), null);
		slots.put(SymbolStructImpl.valueOf("E"), null);
	}
}
