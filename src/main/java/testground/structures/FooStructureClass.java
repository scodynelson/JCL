/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.lang.PackageStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StructureClassStruct;

@SuppressWarnings("all")
public class FooStructureClass extends StructureClassStruct {

	public static final FooStructureClass INSTANCE;

	static {
		final PackageStruct namePackage = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct nameSymbol = namePackage.findSymbol("FOO").getSymbol();

		final PackageStruct constructorPackage = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct constructorSymbol = constructorPackage.findSymbol("MAKE-FOO").getSymbol();

		INSTANCE = new FooStructureClass(nameSymbol, constructorSymbol, null);
		StructureClassStruct.setStructureClass(nameSymbol, INSTANCE);
	}

	protected FooStructureClass(final SymbolStruct name, final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol) {
		super(name, defaultConstructorSymbol, printerSymbol);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new FooStructureObject();
	}
}
