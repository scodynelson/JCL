/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;

public class TestLambdaGenerator extends FunctionStruct {

	private static final long serialVersionUID = 5616713628691655052L;

	private static final LispStruct LTV_1 = new CharacterStruct(1997);

	public TestLambdaGenerator() {
		this(null);
	}

	protected TestLambdaGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLambdaListBindings();
	}

	private void initLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");

		// Start: Required
		final SymbolStruct<?> requiredSymbol = pkg.findSymbol("REQUIRED-SYMBOL").getSymbol();
		ParameterAllocation allocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(requiredSymbol, allocation);
		requiredBindings.add(requiredBinding);
		// End: Required

		// Start: Optional
		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		final SymbolStruct<?> optionalSuppliedPSymbol = pkg.findSymbol("OPTIONAL-SUPPLIED-P-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(2);
		final SuppliedPBinding optionalSuppliedPBinding = new SuppliedPBinding(optionalSuppliedPSymbol, allocation);

		final SymbolStruct<?> optionalSymbol = pkg.findSymbol("OPTIONAL-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(1);
		final LispStruct optionalInitForm = new CharacterStruct(100);

		final OptionalBinding optionalBinding = new OptionalBinding(optionalSymbol, allocation, optionalInitForm, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);
		// End: Optional

		// Start: Rest
		final SymbolStruct<?> restSymbol = pkg.findSymbol("REST-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(3);
		final RestBinding restBinding = new RestBinding(restSymbol, allocation);
		// End: Rest

		// Start: Keys
		final List<KeyBinding> keyBindings = new ArrayList<>();

		final SymbolStruct<?> keySuppliedPSymbol = pkg.findSymbol("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(5);
		final SuppliedPBinding keySuppliedPBinding = new SuppliedPBinding(keySuppliedPSymbol, allocation);

		final SymbolStruct<?> keySymbol = pkg.findSymbol("KEY-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(4);
		final LispStruct keyInitForm = new CharacterStruct(200);

		final PackageStruct keywordPkg = GlobalPackageStruct.KEYWORD;
		final KeywordStruct keyName = (KeywordStruct) keywordPkg.findSymbol("KEY-NAME").getSymbol();

		final KeyBinding keyBinding = new KeyBinding(keySymbol, allocation, keyInitForm, keyName, keySuppliedPBinding);
		keyBindings.add(keyBinding);
		// End: Keys

		// Start: Allow-Other-Keys
		final boolean allowOtherKeys = true;
		// End: Allow-Other-Keys

		// Start: Aux
		final List<AuxBinding> auxBindings = new ArrayList<>();

		final SymbolStruct<?> auxSymbol = pkg.findSymbol("AUX-SYMBOL").getSymbol();
		allocation = new ParameterAllocation(4);
		final LispStruct auxInitForm = new CharacterStruct(300);

		final AuxBinding auxBinding = new AuxBinding(auxSymbol, allocation, auxInitForm);
		auxBindings.add(auxBinding);
		// End: Aux

		lambdaListBindings = new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final Map<SymbolStruct<?>, LispStruct> closureSymbolsToBind = getClosureBindings();
		for (final Map.Entry<SymbolStruct<?>, LispStruct> closureSymbolToBind : closureSymbolsToBind.entrySet()) {
			final SymbolStruct symbol = closureSymbolToBind.getKey();
			LispStruct value = closureSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final Map<SymbolStruct<?>, LispStruct> parameterSymbolsToBind = getFunctionBindings(lispStructs);
		for (final Map.Entry<SymbolStruct<?>, LispStruct> parameterSymbolToBind : parameterSymbolsToBind.entrySet()) {
			final SymbolStruct symbol = parameterSymbolToBind.getKey();
			LispStruct value = parameterSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final LispStruct result;
		try {
//			result = new CharacterStruct(97);
			result = new TestGroundLambdaFunction(closure);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final SymbolStruct<?> parameterSymbolToBind : parameterSymbolsToBind.keySet()) {
				parameterSymbolToBind.unbindLexicalValue();
			}
			for (final SymbolStruct<?> closureSymbolToUnbind : closureSymbolsToBind.keySet()) {
				closureSymbolToUnbind.unbindLexicalValue();
			}
		}
		return result;
	}
}
