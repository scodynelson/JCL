/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.real.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.real.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.real.environment.binding.lambdalist.RestParameter;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.Closure;
import jcl.functions.MacroFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

@SuppressWarnings("all")
public class TestGroundMacroLambdaFunctionGenerator extends MacroFunctionStruct {

	private static final long serialVersionUID = -1939696402314939143L;

	public TestGroundMacroLambdaFunctionGenerator() {
		this(null);
	}

	protected TestGroundMacroLambdaFunctionGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLambdaListBindings();

		// TODO: init the expander
	}

	@Override
	protected void initLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");

		// Start: Required
		final SymbolStruct<?> requiredSymbol = pkg.findSymbol("REQUIRED-SYMBOL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(requiredSymbol, true);
		requiredBindings.add(requiredBinding);
		// End: Required

		// Start: Optional
		final List<OptionalParameter> optionalBindings = new ArrayList<>();

		final SymbolStruct<?> optionalSuppliedPSymbol = pkg.findSymbol("OPTIONAL-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPParameter optionalSuppliedPBinding = new SuppliedPParameter(optionalSuppliedPSymbol, false);

		final SymbolStruct<?> optionalSymbol = pkg.findSymbol("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = new CharacterStruct(100);

		final OptionalParameter optionalBinding = new OptionalParameter(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);
		// End: Optional

		// Start: Rest
		final SymbolStruct<?> restSymbol = pkg.findSymbol("REST-SYMBOL").getSymbol();
		final RestParameter restBinding = new RestParameter(restSymbol, true);
		// End: Rest

		// Start: Keys
		final List<KeyParameter> keyBindings = new ArrayList<>();

		final SymbolStruct<?> keySuppliedPSymbol = pkg.findSymbol("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPParameter keySuppliedPBinding = new SuppliedPParameter(keySuppliedPSymbol, false);

		final SymbolStruct<?> keySymbol = pkg.findSymbol("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = new CharacterStruct(200);

		final SymbolStruct<?> keyName = pkg.findSymbol("KEY-NAME").getSymbol();

		final KeyParameter keyBinding = new KeyParameter(keySymbol, keyInitForm, false, keyName, keySuppliedPBinding);
		keyBindings.add(keyBinding);
		// End: Keys

		// Start: Allow-Other-Keys
		final boolean allowOtherKeys = true;
		// End: Allow-Other-Keys

		// Start: Aux
		final List<AuxParameter> auxBindings = new ArrayList<>();

		final SymbolStruct<?> auxSymbol = pkg.findSymbol("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = new CharacterStruct(300);

		final AuxParameter auxBinding = new AuxParameter(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);
		// End: Aux

		// TODO: MacroLambdaListBindings
//		macroLambdaListBindings = new MacroLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	protected LispStruct internalApply(final Closure currentClosure) {
		return macroFunctionExpander.expand(null, null);
	}

	@Override
	protected LispStruct getInitForm(final Closure currentClosure, final SymbolStruct<?> symbolBinding) {

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
