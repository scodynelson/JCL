/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

@SuppressWarnings("all")
public class TestLambdaGenerator extends FunctionStruct {

	private static final long serialVersionUID = 5616713628691655052L;

	private LispStruct ltv_1;

	public TestLambdaGenerator() {
		this(null);
	}

	protected TestLambdaGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLoadTimeValueForms(closure);
		initLambdaListBindings();
	}

	private void initLoadTimeValueForms(final Closure currentClosure) {
		ltv_1 = new CharacterStruct(1997);
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

		lambdaListBindings = new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	protected LispStruct internalApply(final Closure currentClosure) {
//		result = new CharacterStruct(97);
		return new TestGroundLambdaFunction(currentClosure);
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
