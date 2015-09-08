/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.structures.StructureObjectStruct;
import jcl.symbols.SymbolStruct;

public class BarStructureObject extends StructureObjectStruct {

	private static final long serialVersionUID = -8377552736219667545L;

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, new SymbolStruct<>("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(new SymbolStruct<>("C"), null);
		slots.put(new SymbolStruct<>("D"), null);
		slots.put(new SymbolStruct<>("E"), null);
	}
}
