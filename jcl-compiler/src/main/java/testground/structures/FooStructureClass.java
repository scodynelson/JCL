/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.StructureClassStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStructImpl;
import jcl.type.LispType;

@SuppressWarnings("all")
public class FooStructureClass extends StructureClassStruct {

	public static final FooStructureClass INSTANCE
			= new FooStructureClass(SymbolStructImpl.valueOf("MAKE-FOO"), null, null, null);

	protected FooStructureClass(final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(FooStructureType.INSTANCE, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
	}

	protected FooStructureClass(final LispType type, final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
		SymbolStructImpl.valueOf("FOO").setStructureClass(INSTANCE);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new FooStructureObject();
	}
}
