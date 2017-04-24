/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import jcl.compiler.function.Closure;
import jcl.compiler.function.expanders.SymbolMacroExpander;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.generator.GoException;
import jcl.compiler.icg.generator.ReturnFromException;
import jcl.compiler.icg.generator.ThrowException;
import jcl.lang.CharacterStruct;
import jcl.lang.ComplexStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.factory.LispStructFactory;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.objectweb.asm.Label;

@SuppressWarnings("all")
public class TestGround {

	private SymbolStruct UNINTERNED_SYMBOL = LispStructFactory.toSymbol("FOO");

	private Object blockGen(final Closure currentClosure) {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct name = pkg.findSymbol("FOO").getSymbol();

		LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(97);
		} catch (final ReturnFromException rte) {
			final SymbolStruct rteName = rte.getName();
			if (rteName.eq(name)) {
				result = rte.getResult();
			} else {
				throw rte;
			}
		}
		return result;
	}

	private Object returnFromGen(final Closure currentClosure) {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct name = pkg.findSymbol("FOO").getSymbol();

		final LispStruct result = CharacterStruct.toLispCharacter(97);

		throw new ReturnFromException(name, result);
	}

	private Object ifGen(final Closure currentClosure) {

		LispStruct testObj = CharacterStruct.toLispCharacter(97);
		testObj = ValuesStruct.extractPrimaryValue(testObj);

		final LispStruct result;
		if (!testObj.eq(NILStruct.INSTANCE)) {
			result = CharacterStruct.toLispCharacter(197);
		} else {
			result = CharacterStruct.toLispCharacter(297);
		}
		return result;
	}

	private Object catchGen(final Closure currentClosure) {

		final LispStruct catchTag = CharacterStruct.toLispCharacter(97);

		LispStruct resultForm;
		try {
			resultForm = CharacterStruct.toLispCharacter(197);
		} catch (final ThrowException te) {
			final LispStruct teCatchTag = te.getCatchTag();
			if (teCatchTag.eq(catchTag)) {
				resultForm = te.getResultForm();
			} else {
				throw te;
			}
		}
		return resultForm;
	}

	private Object throwGen(final Closure currentClosure) {

		final LispStruct catchTag = CharacterStruct.toLispCharacter(97);
		final LispStruct resultForm = CharacterStruct.toLispCharacter(197);

		throw new ThrowException(catchTag, resultForm);
	}

	private Object tGen() {
		return TStruct.INSTANCE;
	}

	private Object nilGen() {
		return NILStruct.INSTANCE;
	}

	private Object characterGen() {
		CharacterStruct.toLispCharacter(-1);
		CharacterStruct.toLispCharacter(Integer.MAX_VALUE);
		return CharacterStruct.toLispCharacter(Character.MAX_VALUE);
	}

	private Object floatGen() {
		final Apfloat apfloat = new Apfloat("0");
		return LispStructFactory.toFloat(apfloat);
	}

	private Object integerGen() {
		final Apint apint = new Apint("0");
		return LispStructFactory.toInteger(apint);
	}

	private Object ratioGen() {
		final Aprational aprational = new Aprational("0");
		return LispStructFactory.toRatio(aprational);
	}

	private Object complexGen() {
		final Apcomplex apcomplex = new Apcomplex("0");
		final ComplexStruct.ValueType valueType = ComplexStruct.ValueType.FLOAT;
		return LispStructFactory.toComplex(apcomplex, valueType);
	}

	private Object valuesGen() {
		final List<LispStruct> valuesList = new ArrayList<>();
		final LispStruct value = CharacterStruct.toLispCharacter(97);
		valuesList.add(value);

		return ValuesStruct.valueOf(valuesList);
	}

	private Object consGen() {
		final LispStruct car = CharacterStruct.toLispCharacter(97);
		final LispStruct cdr = CharacterStruct.toLispCharacter(197);
		return LispStructFactory.toCons(car, cdr);
	}

	private Object pathnameGen() {
		final URI uri = URI.create("");
		return LispStructFactory.toPathname(uri);
	}

	private Object stringGen() {
		return StringStruct.toLispString("string");
	}

	private Object bitVectorGen() {
		final List<IntegerStruct> contents = new ArrayList<>();
		final IntegerStruct content = LispStructFactory.toInteger(BigInteger.ZERO);
		contents.add(content);

		return LispStructFactory.toBitVector(contents);
	}

	private Object vectorGen() {
		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = LispStructFactory.toInteger(BigInteger.ZERO);
		contents.add(content);

		return LispStructFactory.toVector(contents);
	}

	private Object arrayGen() {
		final List<Integer> dimensions = new ArrayList<>();
		final int dimension = 1234123412;
		dimensions.add(dimension);

		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = LispStructFactory.toInteger(BigInteger.ZERO);
		contents.add(content);

		return LispStructFactory.toArray(dimensions, contents);
	}

	private Object unwindProtectGen(final Closure currentClosure) {
		final LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(97);
		} finally {
			CharacterStruct.toLispCharacter(197);
		}
		return result;
	}

	private Object goGen(final Closure currentClosure) {

		final int tagIndex = 1234413124;
		throw new GoException(tagIndex);
	}

	private Object symbolGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		return symbol.getValue();
	}

	private Object uninternedSymbolGen() {
		final SymbolStruct symbol = UNINTERNED_SYMBOL;
		return symbol;
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object setqGen(final Closure currentClosure) {

		Map<SymbolStruct, LispStruct> closureBindings = null;
		if (currentClosure != null) {
			closureBindings = currentClosure.getSymbolBindings();
		}

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		LispStruct value = CharacterStruct.toLispCharacter(97);
		value = ValuesStruct.extractPrimaryValue(value);
		symbol.setValue(value);
		if (closureBindings != null) {
			closureBindings.put(symbol, value);
		}

		return value;
	}

	private int tagbodyGen(final Closure currentClosure) {

		final GeneratorState.TagbodyLabel tagbodyLabel = new GeneratorState.TagbodyLabel(null, 20, new Label());
		final int index = tagbodyLabel.getIndex();
		return index;
	}

	private Object quoteListGen() {

		final LispStruct element1 = CharacterStruct.toLispCharacter(97);
		final LispStruct element2 = CharacterStruct.toLispCharacter(197);
		return LispStructFactory.toCons(element1, element2);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object letGen(Closure currentClosure) {

		currentClosure = new Closure(currentClosure);
		final Map<SymbolStruct, LispStruct> closureBindings = currentClosure.getSymbolBindings();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		LispStruct initForm = CharacterStruct.toLispCharacter(97);
		initForm = ValuesStruct.extractPrimaryValue(initForm);
		symbol.bindLexicalValue(initForm);
		closureBindings.put(symbol, initForm);

		final LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(197);
		} finally {
			symbol.unbindLexicalValue();
		}
		return result;
	}

	private Object symbolMacroletGen(final Closure currentClosure) {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		final SymbolMacroExpander symbolMacroExpander = new TestGroundSymbolMacroExpander();
		symbol.bindSymbolMacroExpander(symbolMacroExpander);

		final LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(197);
		} finally {
			symbol.unbindSymbolMacroExpander();
		}
		return result;
	}

	private Object symbolFunctionGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		return symbol.getFunction();
	}

	private Object lambdaFunctionGen(final Closure currentClosure) {

		final FunctionStruct function = new TestGroundLambdaFunction(currentClosure);
		return function;
	}

	private Object functionCallGen(final Closure currentClosure) {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		final FunctionStruct function = symbol.getFunction();

		final LispStruct[] args = new LispStruct[12345678];
		final CharacterStruct arg1 = CharacterStruct.toLispCharacter(97);
		args[1234677] = arg1;

		return function.apply(args);
	}

	private Object lambdaFunctionCallGen(final Closure currentClosure) {

		final FunctionStruct function = new TestGroundLambdaFunction(currentClosure);

		final LispStruct[] args = new LispStruct[12345678];
		final CharacterStruct arg1 = CharacterStruct.toLispCharacter(97);
		args[1234677] = arg1;

		return function.apply(args);
	}

	private Object innerLambdaGen(final Closure currentClosure) {

		Map<SymbolStruct, FunctionStruct> closureBindings = null;
		if (currentClosure != null) {
			closureBindings = currentClosure.getFunctionBindings();
		}

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		final FunctionStruct initForm = new TestGroundLambdaFunction(currentClosure);
		symbol.bindFunction(initForm);
		if (closureBindings != null) {
			closureBindings.put(symbol, initForm);
		}

		final LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(197);
		} finally {
			symbol.unbindFunction();
		}
		return result;
	}

	private Object multipleValueProg1Gen(final Closure currentClosure) {

		final LispStruct firstForm = CharacterStruct.toLispCharacter(97);
		final LispStruct forms = CharacterStruct.toLispCharacter(197);
		return firstForm;
	}

	private Object multipleValueCallGen(final Closure currentClosure) {

		final LispStruct firstForm = CharacterStruct.toLispCharacter(97);
		if (!(firstForm instanceof FunctionStruct)) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid function form: " + firstForm);
		}

		final FunctionStruct functionForm = (FunctionStruct) firstForm;

		final List<LispStruct> argsList = new ArrayList<>();
		final LispStruct form1 = CharacterStruct.toLispCharacter(197);
		ValuesStruct.addValuesToList(argsList, form1);

		LispStruct[] args = new LispStruct[argsList.size()];
		args = argsList.toArray(args);
		return functionForm.apply(args);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object progvGen(Closure currentClosure) {

		final LispStruct vars = null;
		if (!(vars instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Symbols list must be a list. Got: " + vars);
		}

		final ListStruct varsAsList = (ListStruct) vars;
		final List<LispStruct> varsAsJavaList = varsAsList.stream().collect(Collectors.toList());
		for (final LispStruct currentVar : varsAsJavaList) {
			if (!(currentVar instanceof SymbolStruct)) {
				throw new ProgramErrorException("PROGV: Elements in symbols list must be symbols. Got: " + currentVar);
			}
		}

		final LispStruct vals = null;
		if (!(vals instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be a list. Got: " + vals);
		}

		final ListStruct valsAsList = (ListStruct) vals;
		final List<LispStruct> valsAsJavaList = valsAsList.stream().collect(Collectors.toList());

		final int numberOfProgvVars = varsAsJavaList.size();
		final int numberOfProgvVals = valsAsJavaList.size();
		for (int i = 0; i < numberOfProgvVars; i++) {

			// NOTE: We can safely cast here since we checked the type earlier
			final SymbolStruct var = (SymbolStruct) varsAsJavaList.get(i);

			LispStruct val = null;
			if (i < numberOfProgvVals) {
				val = valsAsJavaList.get(i);
			}
			val = ValuesStruct.extractPrimaryValue(val);

			var.bindDynamicValue(val);
		}

		final LispStruct result;
		try {
			result = CharacterStruct.toLispCharacter(197);
		} finally {
			for (final LispStruct var : varsAsJavaList) {
				// NOTE: We can safely cast here since we checked the type earlier
				final SymbolStruct varSymbol = (SymbolStruct) var;
				varSymbol.unbindDynamicValue();
			}
		}
		return result;
	}

	private Object setUpMacroLambda() {
		final TestGroundMacroFunctionExpanderGenerator expanderGenerator = new TestGroundMacroFunctionExpanderGenerator();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct macroName = pkg.findSymbol("FOO").getSymbol();

		macroName.setMacroFunctionExpander(expanderGenerator);
		return expanderGenerator;
	}
}
