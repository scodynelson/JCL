/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.BitVectorStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.generator.specialoperator.TagbodyLabel;
import jcl.compiler.real.icg.generator.specialoperator.exception.GoException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ReturnFromException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ThrowException;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.PackageStruct;
import jcl.pathnames.PathnameFileStruct;
import jcl.pathnames.PathnameURIStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.objectweb.asm.Label;

public class TestGround {

	private static final LispStruct LTV_1 = new CharacterStruct(1997);

	private Object blockGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> name = pkg.findSymbol("FOO").getSymbol();

		LispStruct result;
		try {
			result = new CharacterStruct(97);
		} catch (final ReturnFromException rte) {
			final SymbolStruct<?> rteName = rte.getName();
			if (rteName.equals(name)) {
				result = rte.getResult();
			} else {
				throw rte;
			}
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

		LispStruct testObj = new CharacterStruct(97);
		if (testObj instanceof ValuesStruct) {
			final ValuesStruct valuesStruct = (ValuesStruct) testObj;
			testObj = valuesStruct.getPrimaryValue();
		}

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
			if (teCatchTag.equals(catchTag)) {
				resultForm = te.getResultForm();
			} else {
				throw te;
			}
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

	private Object valuesGen() {
		final List<LispStruct> valuesList = new ArrayList<>();
		final LispStruct value = new CharacterStruct(97);
		valuesList.add(value);

		return new ValuesStruct(valuesList);
	}

	private Object complexGen() {
		final IntegerStruct real = new IntegerStruct(BigInteger.ONE);
		final IntegerStruct imaginary = new IntegerStruct(BigInteger.ZERO);
		return new ComplexStruct(real, imaginary);
	}

	private Object consGen() {

		final LispStruct car = new CharacterStruct(97);
		final LispStruct cdr = new CharacterStruct(197);

		return new ConsStruct(car, cdr);
	}

	private Object pathnameGen() {
		final Path path = Paths.get("");

		new PathnameURIStruct(path);
		return new PathnameFileStruct(path);
	}

	private Object bitVectorGen() {
		final List<IntegerStruct> contents = new ArrayList<>();
		final IntegerStruct content = new IntegerStruct(BigInteger.ZERO);
		contents.add(content);

		return new BitVectorStruct(contents);
	}

	private Object vectorGen() {
		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = new IntegerStruct(BigInteger.ZERO);
		contents.add(content);

		return new VectorStruct<>(contents);
	}

	private Object arrayGen() {
		final List<Integer> dimensions = new ArrayList<>();
		final int dimension = 1234123412;
		dimensions.add(dimension);

		final List<LispStruct> contents = new ArrayList<>();
		final IntegerStruct content = new IntegerStruct(BigInteger.ZERO);
		contents.add(content);

		return new ArrayStruct<>(dimensions, contents);
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
	private Object setqGen(final FunctionStruct function) {

		final Closure currentClosure = function.getClosure();
		Map<SymbolStruct<?>, LispStruct> closureBindings = null;
		if (currentClosure != null) {
			closureBindings = currentClosure.getSymbolBindings();
		}

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		LispStruct value = new CharacterStruct(97);
		if (value instanceof ValuesStruct) {
			final ValuesStruct valuesStruct = (ValuesStruct) value;
			value = valuesStruct.getPrimaryValue();
		}
		symbol.setValue(value);
		if (closureBindings != null) {
			closureBindings.put(symbol, value);
		}

		return value;
	}

	private int tagbodyGen() {

		final TagbodyLabel tagbodyLabel = new TagbodyLabel(null, 20, new Label());
		final int index = tagbodyLabel.getIndex();
		return index;
	}

	private Object quoteListGen() {

		final LispStruct element1 = new CharacterStruct(97);
		final LispStruct element2 = new CharacterStruct(197);
		return new ConsStruct(element1, element2);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private Object letGen(final FunctionStruct function) {

		final Closure parentClosure = function.getClosure();
		final Closure closure = new Closure(parentClosure);
		final Map<SymbolStruct<?>, LispStruct> closureBindings = closure.getSymbolBindings();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol = pkg.findSymbol("FOO").getSymbol();

		LispStruct initForm = new CharacterStruct(97);
		if (initForm instanceof ValuesStruct) {
			final ValuesStruct valuesStruct = (ValuesStruct) initForm;
			initForm = valuesStruct.getPrimaryValue();
		}
		symbol.bindLexicalValue(initForm);
		closureBindings.put(symbol, initForm);

		final LispStruct result;
		try {
			result = new CharacterStruct(197);
		} finally {
			symbol.unbindLexicalValue();
		}
		return result;
	}

	private Object symbolMacroletGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol = pkg.findSymbol("FOO").getSymbol();

		final SymbolMacroExpander<?> symbolMacroExpander = new TestGroundSymbolMacroExpander();
		symbol.bindSymbolMacroExpander(symbolMacroExpander);

		final LispStruct result;
		try {
			result = new CharacterStruct(197);
		} finally {
			symbol.unbindSymbolMacroExpander();
		}
		return result;
	}

	private Object symbolFunctionGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol = pkg.findSymbol("FOO").getSymbol();

		return symbol.getFunction();
	}

	private Object lambdaFunctionGen() {
		final Closure parentClosure = null;

		final FunctionStruct function = new TestGroundLambdaFunction(parentClosure);
		return function;
	}

	private Object functionCallGen() {

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol = pkg.findSymbol("FOO").getSymbol();

		final FunctionStruct function = symbol.getFunction();

		final LispStruct[] args = new LispStruct[12345678];
		final CharacterStruct arg1 = new CharacterStruct(97);
		args[1234677] = arg1;

		return function.apply(args);
	}

	private Object lambdaFunctionCallGen() {
		final Closure parentClosure = null;

		final FunctionStruct function = new TestGroundLambdaFunction(parentClosure);

		final LispStruct[] args = new LispStruct[12345678];
		final CharacterStruct arg1 = new CharacterStruct(97);
		args[1234677] = arg1;

		return function.apply(args);
	}

	private Object fletGen(final FunctionStruct function) {

		final Closure currentClosure = function.getClosure();
		Map<SymbolStruct<?>, FunctionStruct> closureBindings = null;
		if (currentClosure != null) {
			closureBindings = currentClosure.getFunctionBindings();
		}

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol = pkg.findSymbol("FOO").getSymbol();

		final FunctionStruct initForm = new TestGroundLambdaFunction(currentClosure);
		symbol.bindFunction(initForm);
		if (closureBindings != null) {
			closureBindings.put(symbol, initForm);
		}

		final LispStruct result;
		try {
			result = new CharacterStruct(197);
		} finally {
			symbol.unbindFunction();
		}
		return result;
	}

	private Object ltvGen() {
		return LTV_1;
	}

	private Object multipleValueProg1Gen() {

		final LispStruct firstForm = new CharacterStruct(97);
		final LispStruct forms = new CharacterStruct(197);
		return firstForm;
	}

	private Object multipleValueCallGen() {

		final LispStruct firstForm = new CharacterStruct(97);
		if (!(firstForm instanceof FunctionStruct)) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid function form: " + firstForm);
		}

		final FunctionStruct functionForm = (FunctionStruct) firstForm;

		final List<LispStruct> argsList = new ArrayList<>();
		final LispStruct form1 = new CharacterStruct(197);
		if (form1 instanceof ValuesStruct) {
			final List<LispStruct> valuesList = ((ValuesStruct) form1).getValuesList();
			for (final LispStruct value : valuesList) {
				argsList.add(value);
			}
		} else {
			argsList.add(form1);
		}

		LispStruct[] args = new LispStruct[argsList.size()];
		args = argsList.toArray(args);
		return functionForm.apply(args);
	}
}
