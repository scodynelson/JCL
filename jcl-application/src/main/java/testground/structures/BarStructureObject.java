/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class BarStructureObject extends StructureObjectStruct {

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, new SymbolStruct("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(new SymbolStruct("C"), null);
		slots.put(new SymbolStruct("D"), null);
		slots.put(new SymbolStruct("E"), null);
	}
}
