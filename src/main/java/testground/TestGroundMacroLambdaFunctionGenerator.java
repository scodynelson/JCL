/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionParameterBinding;
import jcl.functions.FunctionStruct;
import jcl.functions.MacroFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

@SuppressWarnings("all")
public class TestGroundMacroLambdaFunctionGenerator extends MacroFunctionStruct {

	private static final long serialVersionUID = -1939696402314939143L;

	private static final LispStruct LTV_1 = new CharacterStruct(1997);

	public TestGroundMacroLambdaFunctionGenerator() {
		this(null);
	}

	protected TestGroundMacroLambdaFunctionGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLambdaListBindings();

		// TODO: init the expander
	}

	private void initLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");

		// Start: Required
		final SymbolStruct<?> requiredSymbol = pkg.findSymbol("REQUIRED-SYMBOL").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(requiredSymbol, true);
		requiredBindings.add(requiredBinding);
		// End: Required

		// Start: Optional
		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		final SymbolStruct<?> optionalSuppliedPSymbol = pkg.findSymbol("OPTIONAL-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPBinding optionalSuppliedPBinding = new SuppliedPBinding(optionalSuppliedPSymbol, false);

		final SymbolStruct<?> optionalSymbol = pkg.findSymbol("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = new CharacterStruct(100);

		final OptionalBinding optionalBinding = new OptionalBinding(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);
		// End: Optional

		// Start: Rest
		final SymbolStruct<?> restSymbol = pkg.findSymbol("REST-SYMBOL").getSymbol();
		final RestBinding restBinding = new RestBinding(restSymbol, true);
		// End: Rest

		// Start: Keys
		final List<KeyBinding> keyBindings = new ArrayList<>();

		final SymbolStruct<?> keySuppliedPSymbol = pkg.findSymbol("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPBinding keySuppliedPBinding = new SuppliedPBinding(keySuppliedPSymbol, false);

		final SymbolStruct<?> keySymbol = pkg.findSymbol("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = new CharacterStruct(200);

		final SymbolStruct<?> keyName = pkg.findSymbol("KEY-NAME").getSymbol();

		final KeyBinding keyBinding = new KeyBinding(keySymbol, keyInitForm, false, keyName, keySuppliedPBinding);
		keyBindings.add(keyBinding);
		// End: Keys

		// Start: Allow-Other-Keys
		final boolean allowOtherKeys = true;
		// End: Allow-Other-Keys

		// Start: Aux
		final List<AuxBinding> auxBindings = new ArrayList<>();

		final SymbolStruct<?> auxSymbol = pkg.findSymbol("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = new CharacterStruct(300);

		final AuxBinding auxBinding = new AuxBinding(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);
		// End: Aux

		// TODO: MacroLambdaListBindings
//		macroLambdaListBindings = new MacroLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final Map<SymbolStruct<?>, LispStruct> closureSymbolsToBind = getClosureSymbolBindings();
		for (final Map.Entry<SymbolStruct<?>, LispStruct> closureSymbolToBind : closureSymbolsToBind.entrySet()) {
			final SymbolStruct symbol = closureSymbolToBind.getKey();
			LispStruct value = closureSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final Map<SymbolStruct<?>, FunctionStruct> closureFunctionsToBind = getClosureFunctionBindings();
		for (final Map.Entry<SymbolStruct<?>, FunctionStruct> closureFunctionToBind : closureFunctionsToBind.entrySet()) {
			final SymbolStruct symbol = closureFunctionToBind.getKey();
			final FunctionStruct function = closureFunctionToBind.getValue();
			symbol.bindFunction(function);
		}

		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(lispStructs);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStruct symbol = parameterSymbolToBind.getParameterSymbol();
			LispStruct value = parameterSymbolToBind.getParameterValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			} else if (INIT_FORM_PLACEHOLDER.equals(value)) {
				value = getInitForm(symbol);
			}
			final boolean isSpecial = parameterSymbolToBind.isSpecial();
			if (isSpecial) {
				symbol.bindDynamicValue(value);
			} else {
				symbol.bindLexicalValue(value);
			}
		}

		final LispStruct result;
		try {
			result = internalApply(closure);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final FunctionParameterBinding parameterSymbolToUnbind : parameterSymbolsToBind) {
				final SymbolStruct<?> parameterSymbol = parameterSymbolToUnbind.getParameterSymbol();
				final boolean isSpecial = parameterSymbolToUnbind.isSpecial();
				if (isSpecial) {
					parameterSymbol.unbindDynamicValue();
				} else {
					parameterSymbol.unbindLexicalValue();
				}
			}
			for (final SymbolStruct<?> closureFunctionToUnbind : closureFunctionsToBind.keySet()) {
				closureFunctionToUnbind.unbindFunction();
			}
			for (final SymbolStruct<?> closureSymbolToUnbind : closureSymbolsToBind.keySet()) {
				closureSymbolToUnbind.unbindLexicalValue();
			}
		}
		return result;
	}

	private LispStruct internalApply(final Closure currentClosure) {
		return macroFunctionExpander.expand(null, null);
	}

	private LispStruct getInitForm(final SymbolStruct<?> symbolBinding) {

		final PackageStruct pkg1 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol1 = pkg1.findSymbol("SYMBOL1").getSymbol();
		if (symbolBinding.equals(symbol1)) {
			return new CharacterStruct(100);
		}

		final PackageStruct pkg2 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> symbol2 = pkg2.findSymbol("SYMBOL2").getSymbol();
		if (symbolBinding.equals(symbol2)) {
			return new CharacterStruct(200);
		}

		return NullStruct.INSTANCE;
	}
}
