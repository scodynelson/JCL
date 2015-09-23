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
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@SuppressWarnings("all")
@Component
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

	@Override
	protected void initLoadTimeValueForms(final Closure currentClosure) {
		ltv_1 = new CharacterStruct(1997);
	}

	@Override
	protected List<RequiredBinding> getRequiredBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> requiredSymbol = pkg.intern("REQUIRED-SYMBOL").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(requiredSymbol, true);
		requiredBindings.add(requiredBinding);

		return requiredBindings;
	}

	@Override
	protected List<OptionalBinding> getOptionalBindings() {
		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> optionalSymbol = pkg.intern("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = NullStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> optionalSuppliedPSymbol = pkg.intern("OPTIONAL-SYMBOL-P").getSymbol();
		final SuppliedPBinding optionalSuppliedPBinding = new SuppliedPBinding(optionalSuppliedPSymbol, false);

		final OptionalBinding optionalBinding = new OptionalBinding(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);

		return optionalBindings;
	}

	@Override
	protected RestBinding getRestBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> restSymbol = pkg.intern("REST-SYMBOL").getSymbol();
		return new RestBinding(restSymbol, true);
	}

	@Override
	protected List<KeyBinding> getKeyBindings() {
		final List<KeyBinding> keyBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> keySymbol = pkg.intern("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = NullStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> keySuppliedPSymbol = pkg.intern("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPBinding keySuppliedPBinding = new SuppliedPBinding(keySuppliedPSymbol, false);

		pkg = PackageStruct.findPackage("KEYWORD");
		final SymbolStruct<?> keyName = pkg.intern("KEY-NAME").getSymbol();

		final KeyBinding keyBinding = new KeyBinding(keySymbol, keyInitForm, false, keyName, keySuppliedPBinding);
		keyBindings.add(keyBinding);

		return keyBindings;
	}

	@Override
	protected boolean getAllowOtherKeys() {
		return true;
	}

	@Override
	protected List<AuxBinding> getAuxBindings() {
		final List<AuxBinding> auxBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct<?> auxSymbol = pkg.intern("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = NullStruct.INSTANCE;

		final AuxBinding auxBinding = new AuxBinding(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);

		return auxBindings;
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
