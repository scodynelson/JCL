/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.generator.specialoperator.exception.ReturnFromException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ThrowException;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;

public class TestGround {

	private Object block() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		LispStruct result;
		try {
			result = new CharacterStruct(97);
		} catch (final ReturnFromException rte) {
			final SymbolStruct<?> rteName = rte.getName();
			if (!rteName.equals(name)) {
				throw rte;
			}
			result = rte.getResult();
		}
		return result;
	}

	private void returnFrom() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		final LispStruct result = new CharacterStruct(97);

		throw new ReturnFromException(name, result);
	}

	private Object ifGen() {

		final LispStruct testObj = null;

		final LispStruct result;
		if (!testObj.equals(NullStruct.INSTANCE) && !testObj.equals(NILStruct.INSTANCE)) {
			result = new CharacterStruct(97);
		} else {
			result = new CharacterStruct(197);
		}
		return result;
	}

	private Object catchGen() {

		final LispStruct catchTag = new CharacterStruct(97);

		LispStruct resultForm;
		try {
			resultForm = new CharacterStruct(197);
		} catch (final ThrowException te) {
			final LispStruct teCatchTag = te.getCatchTag();
			if (!teCatchTag.equals(catchTag)) {
				throw te;
			}
			resultForm = te.getResultForm();
		}
		return resultForm;
	}

	private void throwGen() {

		final LispStruct catchTag = new CharacterStruct(97);
		final LispStruct resultForm = new CharacterStruct(197);

		throw new ThrowException(catchTag, resultForm);
	}

}
