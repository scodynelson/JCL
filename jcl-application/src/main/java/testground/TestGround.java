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

import jcl.lang.LispStruct;
import jcl.lang.array.ArrayStruct;
import jcl.lang.array.BitVectorStruct;
import jcl.lang.array.VectorStruct;
import jcl.lang.CharacterStruct;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.generator.GoException;
import jcl.compiler.icg.generator.ReturnFromException;
import jcl.compiler.icg.generator.ThrowException;
import jcl.lang.ValuesStruct;
import jcl.lang.ValuesStructs;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.compiler.function.Closure;
import jcl.lang.function.FunctionStruct;
import jcl.compiler.function.expanders.SymbolMacroExpander;
import jcl.lang.ConsStruct;
import jcl.lang.ListStruct;
import jcl.lang.number.ComplexStruct;
import jcl.lang.number.FloatStruct;
import jcl.lang.number.IntegerStruct;
import jcl.lang.number.RatioStruct;
import jcl.lang.PackageStruct;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.objectweb.asm.Label;

@SuppressWarnings("all")
public class TestGround {

	private SymbolStruct UNINTERNED_SYMBOL = new SymbolStruct("FOO");

	private Object blockGen(final Closure currentClosure) {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct name = pkg.findSymbol("FOO").getSymbol();

		LispStruct result;
		try {
			result = CharacterStruct.valueOf(97);
		} catch (final ReturnFromException rte) {
			final SymbolStruct rteName = rte.getName();
			if (rteName.equals(name)) {
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

		final LispStruct result = CharacterStruct.valueOf(97);

		throw new ReturnFromException(name, result);
	}

	private Object ifGen(final Closure currentClosure) {

		LispStruct testObj = CharacterStruct.valueOf(97);
		testObj = ValuesStructs.extractPrimaryValue(testObj);

		final LispStruct result;
		if (!testObj.equals(NILStruct.INSTANCE)) {
			result = CharacterStruct.valueOf(197);
		} else {
			result = CharacterStruct.valueOf(297);
		}
		return result;
	}

	private Object catchGen(final Closure currentClosure) {

		final LispStruct catchTag = CharacterStruct.valueOf(97);

		LispStruct resultForm;
		try {
			resultForm = CharacterStruct.valueOf(197);
		} catch (final ThrowException te) {
			final LispStruct teCatchTag = te.getCatchTag();
			if (teCatchTag.equals(catchTag)) {
				resultForm = te.getResultForm();
			} else {
				throw te;
			}
		}
		return resultForm;
	}

	private Object throwGen(final Closure currentClosure) {

		final LispStruct catchTag = CharacterStruct.valueOf(97);
		final LispStruct resultForm = CharacterStruct.valueOf(197);

		throw new ThrowException(catchTag, resultForm);
	}

	private Object tGen() {
		return TStruct.INSTANCE;
	}

	private Object nilGen() {
		return NILStruct.INSTANCE;
	}

	private Object characterGen() {
		return CharacterStruct.valueOf(66544564);
	}

	private Object floatGen() {
		final Apfloat apfloat = new Apfloat("0");
		return FloatStruct.valueOf(apfloat);
	}

	private Object integerGen() {
		final Apint apint = new Apint("0");
		return IntegerStruct.valueOf(apint);
	}

	private Object ratioGen() {
		final Aprational aprational = new Aprational("0");
		return RatioStruct.valueOf(aprational);
	}

	private Object complexGen() {
		final Apcomplex apcomplex = new Apcomplex("0");
		final ComplexStruct.ValueType valueType = ComplexStruct.ValueType.FLOAT;
		return ComplexStruct.valueOf(apcomplex, valueType);
	}

	private Object valuesGen() {
		final List<LispStruct> valuesList = new ArrayList<>();
		final LispStruct value = CharacterStruct.valueOf(97);
		valuesList.add(value);

		return new ValuesStruct(valuesList);
	}

	private Object consGen() {
		final LispStruct car = CharacterStruct.valueOf(97);
		final LispStruct cdr = CharacterStruct.valueOf(197);
		return new ConsStruct(car, cdr);
	}

	private Object pathnameGen() {
		final URI uri = URI.create("");
		return new PathnameStruct(uri);
	}

	private Object bitVectorGen() {
		final List<IntegerStruct> contents = new ArrayList<>();
		final IntegerStruct content = IntegerStruct.valueOf(BigInteger.ZERO);
		contents.add(content);

		return new BitVectorStruct(contents);
	}

	private Object vectorGen() {
		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = IntegerStruct.valueOf(BigInteger.ZERO);
		contents.add(content);

		return new VectorStruct<>(contents);
	}

	private Object arrayGen() {
		final List<Integer> dimensions = new ArrayList<>();
		final int dimension = 1234123412;
		dimensions.add(dimension);

		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = IntegerStruct.valueOf(BigInteger.ZERO);
		contents.add(content);

		return new ArrayStruct<>(dimensions, contents);
	}

	private Object unwindProtectGen(final Closure currentClosure) {
		final LispStruct result;
		try {
			result = CharacterStruct.valueOf(97);
		} finally {
			CharacterStruct.valueOf(197);
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

		LispStruct value = CharacterStruct.valueOf(97);
		value = ValuesStructs.extractPrimaryValue(value);
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

		final LispStruct element1 = CharacterStruct.valueOf(97);
		final LispStruct element2 = CharacterStruct.valueOf(197);
		return new ConsStruct(element1, element2);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object letGen(Closure currentClosure) {

		currentClosure = new Closure(currentClosure);
		final Map<SymbolStruct, LispStruct> closureBindings = currentClosure.getSymbolBindings();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		LispStruct initForm = CharacterStruct.valueOf(97);
		initForm = ValuesStructs.extractPrimaryValue(initForm);
		symbol.bindLexicalValue(initForm);
		closureBindings.put(symbol, initForm);

		final LispStruct result;
		try {
			result = CharacterStruct.valueOf(197);
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
			result = CharacterStruct.valueOf(197);
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
		final CharacterStruct arg1 = CharacterStruct.valueOf(97);
		args[1234677] = arg1;

		return function.apply(args);
	}

	private Object lambdaFunctionCallGen(final Closure currentClosure) {

		final FunctionStruct function = new TestGroundLambdaFunction(currentClosure);

		final LispStruct[] args = new LispStruct[12345678];
		final CharacterStruct arg1 = CharacterStruct.valueOf(97);
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
			result = CharacterStruct.valueOf(197);
		} finally {
			symbol.unbindFunction();
		}
		return result;
	}

	private Object multipleValueProg1Gen(final Closure currentClosure) {

		final LispStruct firstForm = CharacterStruct.valueOf(97);
		final LispStruct forms = CharacterStruct.valueOf(197);
		return firstForm;
	}

	private Object multipleValueCallGen(final Closure currentClosure) {

		final LispStruct firstForm = CharacterStruct.valueOf(97);
		if (!(firstForm instanceof FunctionStruct)) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid function form: " + firstForm);
		}

		final FunctionStruct functionForm = (FunctionStruct) firstForm;

		final List<LispStruct> argsList = new ArrayList<>();
		final LispStruct form1 = CharacterStruct.valueOf(197);
		ValuesStructs.addValuesToList(argsList, form1);

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
			val = ValuesStructs.extractPrimaryValue(val);

			var.bindDynamicValue(val);
		}

		final LispStruct result;
		try {
			result = CharacterStruct.valueOf(197);
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
