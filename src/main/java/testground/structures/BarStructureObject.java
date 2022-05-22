/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.StructureObjectStructImpl;

@SuppressWarnings("all")
public class BarStructureObject extends StructureObjectStructImpl {

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, SymbolStruct.toLispSymbol("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStruct.toLispSymbol("C"), NILStruct.INSTANCE);
		slots.put(SymbolStruct.toLispSymbol("D"), NILStruct.INSTANCE);
		slots.put(SymbolStruct.toLispSymbol("E"), NILStruct.INSTANCE);
	}
}
