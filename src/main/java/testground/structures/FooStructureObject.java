/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.StructureObjectStructImpl;

@SuppressWarnings("all")
public class FooStructureObject extends StructureObjectStructImpl {

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, SymbolStruct.toLispSymbol("FOO"), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(SymbolStruct.toLispSymbol("A"), NILStruct.INSTANCE);
		slots.put(SymbolStruct.toLispSymbol("B"), NILStruct.INSTANCE);
		slots.put(SymbolStruct.toLispSymbol("C"), NILStruct.INSTANCE);
	}
}
