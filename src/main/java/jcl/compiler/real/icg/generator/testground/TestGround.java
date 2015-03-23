/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.generator.specialoperator.TagbodyLabel;
import jcl.compiler.real.icg.generator.specialoperator.exception.GoException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ReturnFromException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ThrowException;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.objectweb.asm.Label;

public class TestGround {

	private Object blockGen() {

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

	private Object returnFromGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		final LispStruct result = new CharacterStruct(97);

		throw new ReturnFromException(name, result);
	}

	private Object ifGen() {

		final LispStruct testObj = new CharacterStruct(97);

		final LispStruct result;
		if (!testObj.equals(NullStruct.INSTANCE) && !testObj.equals(NILStruct.INSTANCE)) {
			result = new CharacterStruct(197);
		} else {
			result = new CharacterStruct(297);
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

	private Object throwGen() {

		final LispStruct catchTag = new CharacterStruct(97);
		final LispStruct resultForm = new CharacterStruct(197);

		throw new ThrowException(catchTag, resultForm);
	}

	private Object tGen() {
		return TStruct.INSTANCE;
	}

	private Object nilGen() {
		return NILStruct.INSTANCE;
	}

	private Object nullGen() {
		return NullStruct.INSTANCE;
	}

	private Object characterGen() {
		return new CharacterStruct(66544564);
	}

	private Object floatGen() {
		final BigDecimal bigDecimal = new BigDecimal("12345.0");
		return new FloatStruct(bigDecimal);
	}

	private Object integerGen() {
		final BigInteger bigInteger = new BigInteger("12345");
		return new IntegerStruct(bigInteger);
	}

	private Object ratioGen() {
		final BigInteger numerator = new BigInteger("1");
		final BigInteger denominator = new BigInteger("2");
		return new RatioStruct(numerator, denominator);
	}

	private Object unwindProtectGen() {
		final LispStruct result;
		try {
			result = new CharacterStruct(97);
		} finally {
			new CharacterStruct(197);
		}
		return result;
	}

	private Object goGen() {

		final int tagIndex = 1234413124;

		throw new GoException(tagIndex);
	}

	private Object symbolGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol = pkg.findSymbol("FOO").getSymbol();

		return symbol.getValue();
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object setqGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		final LispStruct value = new CharacterStruct(97);
		symbol.setValue(value);

		return value;
	}

	private int tagbodyGen() {

		final TagbodyLabel tagbodyLabel = new TagbodyLabel(null, 20, new Label());
		final int index = tagbodyLabel.getIndex();
		return index;
	}

	private Object quoteListGen() {

		final LispStruct element1 = new CharacterStruct(97);
		final LispStruct element2 = new CharacterStruct(97);
		return new ConsStruct(element1, element2);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object letGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		final LispStruct initForm = new CharacterStruct(97);
		symbol.bindLexicalValue(initForm);

		final LispStruct result;
		try {
			result = new CharacterStruct(197);
		} finally {
			symbol.unbindLexicalValue();
		}
		return result;
	}
}
