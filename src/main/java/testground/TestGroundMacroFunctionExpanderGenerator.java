/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.functions.Closure;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

@SuppressWarnings("all")
public class TestGroundMacroFunctionExpanderGenerator extends MacroFunctionExpander<LispStruct> {

	public TestGroundMacroFunctionExpanderGenerator() {
		this(null);
	}

	protected TestGroundMacroFunctionExpanderGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLambdaListBindings();
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct requiredSymbol = pkg.intern("REQUIRED-SYMBOL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(requiredSymbol, true);
		requiredBindings.add(requiredBinding);

		return requiredBindings;
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct optionalSymbol = pkg.intern("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = NullStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct optionalSuppliedPSymbol = pkg.intern("OPTIONAL-SYMBOL-P").getSymbol();
		final SuppliedPParameter optionalSuppliedPBinding = new SuppliedPParameter(optionalSuppliedPSymbol, false);

		final OptionalParameter optionalBinding = new OptionalParameter(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);

		return optionalBindings;
	}

	@Override
	protected RestParameter getRestBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct restSymbol = pkg.intern("REST-SYMBOL").getSymbol();
		return new RestParameter(restSymbol, true);
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct keySymbol = pkg.intern("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = NullStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct keySuppliedPSymbol = pkg.intern("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPParameter keySuppliedPBinding = new SuppliedPParameter(keySuppliedPSymbol, false);

		pkg = PackageStruct.findPackage("KEYWORD");
		final SymbolStruct keyName = pkg.intern("KEY-NAME").getSymbol();

		final KeyParameter keyBinding = new KeyParameter(keySymbol, keyInitForm, false, keyName, keySuppliedPBinding);
		keyBindings.add(keyBinding);

		return keyBindings;
	}

	@Override
	protected boolean getAllowOtherKeys() {
		return true;
	}

	@Override
	protected List<AuxParameter> getAuxBindings() {
		final List<AuxParameter> auxBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct auxSymbol = pkg.intern("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = NullStruct.INSTANCE;

		final AuxParameter auxBinding = new AuxParameter(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);

		return auxBindings;
	}

	@Override
	protected WholeParameter getWholeBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct wholeSymbol = pkg.intern("WHOLE-SYMBOL").getSymbol();
		return new WholeParameter(wholeSymbol, true);
	}

	@Override
	protected EnvironmentParameter getEnvironmentBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct environmentSymbol = pkg.intern("ENVIRONMENT-SYMBOL").getSymbol();
		return new EnvironmentParameter(environmentSymbol);
	}

	@Override
	protected BodyParameter getBodyBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct bodySymbol = pkg.intern("BODY-SYMBOL").getSymbol();
		return new BodyParameter(bodySymbol, true);
	}

	@Override
	protected LispStruct internalApply(final Closure currentClosure) {
		return CharacterStruct.valueOf(97);
	}

	@Override
	protected LispStruct getInitForm(final Closure currentClosure, final SymbolStruct symbolBinding) {

		final PackageStruct pkg1 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol1 = pkg1.findSymbol("SYMBOL1").getSymbol();
		if (symbolBinding.equals(symbol1)) {
			return CharacterStruct.valueOf(100);
		}

		final PackageStruct pkg2 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol2 = pkg2.findSymbol("SYMBOL2").getSymbol();
		if (symbolBinding.equals(symbol2)) {
			return CharacterStruct.valueOf(200);
		}

		return NullStruct.INSTANCE;
	}
}
