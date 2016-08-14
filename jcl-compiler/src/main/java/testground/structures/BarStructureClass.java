/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.StructureObjectStructImpl;
import jcl.lang.SymbolStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.type.LispType;

@SuppressWarnings("all")
public class BarStructureClass extends FooStructureClass {

	public static final BarStructureClass INSTANCE
			= new BarStructureClass(LispStructFactory.toSymbol("MAKE-BAR"), null, null, null);

	protected BarStructureClass(final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(BarStructureType.INSTANCE, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
	}

	protected BarStructureClass(final LispType type, final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
		LispStructFactory.toSymbol("BAR").setStructureClass(INSTANCE);
	}

	@Override
	public StructureObjectStructImpl newInstance() {
		return new BarStructureObject();
	}
}
