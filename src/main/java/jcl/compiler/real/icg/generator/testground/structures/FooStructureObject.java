/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import jcl.classes.StructureObjectStruct;
import jcl.compiler.real.icg.generator.testground.TestGroundLambdaFunction;
import jcl.symbols.SymbolStruct;

public class FooStructureObject extends StructureObjectStruct {

	private static final long serialVersionUID = -5612579845485020663L;

	public FooStructureObject() {
		super(FooStructureClass.INSTANCE, new TestGroundLambdaFunction(null), null);
		initSlotsMap();
	}

	private void initSlotsMap() {
		slots.put(new SymbolStruct<>("A"), null);
		slots.put(new SymbolStruct<>("B"), null);
		slots.put(new SymbolStruct<>("C"), null);
	}
}
