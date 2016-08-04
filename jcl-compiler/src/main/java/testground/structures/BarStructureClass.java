/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStructImpl;
import jcl.type.LispType;

@SuppressWarnings("all")
public class BarStructureClass extends FooStructureClass {

	public static final BarStructureClass INSTANCE
			= new BarStructureClass(SymbolStructImpl.valueOf("MAKE-BAR"), null, null, null);

	protected BarStructureClass(final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(BarStructureType.INSTANCE, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
	}

	protected BarStructureClass(final LispType type, final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
		SymbolStructImpl.valueOf("BAR").setStructureClass(INSTANCE);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new BarStructureObject();
	}
}
