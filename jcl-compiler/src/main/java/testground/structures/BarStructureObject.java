/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.StructureObjectStructImpl;
import jcl.lang.factory.LispStructFactory;

@SuppressWarnings("all")
public class BarStructureObject extends StructureObjectStructImpl {

	public BarStructureObject() {
		super(BarStructureClass.INSTANCE, LispStructFactory.toSymbol("BAR"), FooStructureClass.INSTANCE.newInstance());
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(LispStructFactory.toSymbol("C"), null);
		slots.put(LispStructFactory.toSymbol("D"), null);
		slots.put(LispStructFactory.toSymbol("E"), null);
	}
}
