/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.LispType;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class BarStructureClass extends FooStructureClass {

	public static final BarStructureClass INSTANCE
			= new BarStructureClass(new SymbolStruct("MAKE-BAR"), null, null, null);

	protected BarStructureClass(final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(BarStructureType.INSTANCE, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
	}

	protected BarStructureClass(final LispType type, final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
		new SymbolStruct("BAR").setStructureClass(INSTANCE);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new BarStructureObject();
	}
}
