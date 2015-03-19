/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.generator.specialoperator.exception.ReturnFromException;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

public class TestGround {

	private Object block() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		try {
			return new CharacterStruct(97);
		} catch (final ReturnFromException rte) {
			final SymbolStruct<?> rteName = rte.getName();
			if (rteName.equals(name)) {
				return rte.getResult();
			}
			throw rte;
		}
	}

	private void returnFrom() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		throw new ReturnFromException(name, null);
	}
}
