/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.DestructuringLambdaList;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.function.Closure;
import jcl.compiler.function.expanders.CompiledMacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.NILStruct;

@SuppressWarnings("all")
public class TestGroundMacroFunctionExpanderGenerator extends CompiledMacroFunctionExpander<LispStruct> {

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
		final SymbolStructImpl requiredSymbol = pkg.intern("REQUIRED-SYMBOL").getSymbol();
		final DestructuringLambdaList destructuringForm = null;
		final RequiredParameter requiredBinding = new RequiredParameter(requiredSymbol, null, true);
		requiredBindings.add(requiredBinding);

		return requiredBindings;
	}

	// NOTE: This is NOT part of the actual object generated, but will be used in generating destructuring form objects
	private void destructuringFormMethod(final DestructuringLambdaList destructuringForm) {

		final WholeParameter wholeParameter = new WholeParameter(null);
		final List<RequiredParameter> requiredParameters = Collections.emptyList();
		final List<OptionalParameter> optionalParameters = Collections.emptyList();
		final RestParameter restBinding = new RestParameter(null);
		final BodyParameter bodyParameter = new BodyParameter(null);
		final List<KeyParameter> keyParameters = Collections.emptyList();
		final List<AuxParameter> auxParameters = Collections.emptyList();
		final boolean allowOtherKeys = true;

		final DestructuringLambdaList newDestructuringForm
				= new DestructuringLambdaList(wholeParameter, requiredParameters, optionalParameters,
				                              restBinding, bodyParameter, keyParameters,
				                              auxParameters, allowOtherKeys);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl optionalSymbol = pkg.intern("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = NILStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl optionalSuppliedPSymbol = pkg.intern("OPTIONAL-SYMBOL-P").getSymbol();
		final SuppliedPParameter optionalSuppliedPBinding = new SuppliedPParameter(optionalSuppliedPSymbol, false);

		final OptionalParameter optionalBinding = new OptionalParameter(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);

		return optionalBindings;
	}

	@Override
	protected RestParameter getRestBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl restSymbol = pkg.intern("REST-SYMBOL").getSymbol();
		return new RestParameter(restSymbol, true);
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl keySymbol = pkg.intern("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = NILStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl keySuppliedPSymbol = pkg.intern("KEY-SUPPLIED-P-SYMBOL").getSymbol();
		final SuppliedPParameter keySuppliedPBinding = new SuppliedPParameter(keySuppliedPSymbol, false);

		pkg = PackageStruct.findPackage("KEYWORD");
		final SymbolStructImpl keyName = pkg.intern("KEY-NAME").getSymbol();

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
		final SymbolStructImpl auxSymbol = pkg.intern("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = NILStruct.INSTANCE;

		final AuxParameter auxBinding = new AuxParameter(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);

		return auxBindings;
	}

	@Override
	protected WholeParameter getWholeBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl wholeSymbol = pkg.intern("WHOLE-SYMBOL").getSymbol();
		return new WholeParameter(wholeSymbol, true);
	}

	@Override
	protected EnvironmentParameter getEnvironmentBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl environmentSymbol = pkg.intern("ENVIRONMENT-SYMBOL").getSymbol();
		return new EnvironmentParameter(environmentSymbol);
	}

	@Override
	protected BodyParameter getBodyBinding() {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl bodySymbol = pkg.intern("BODY-SYMBOL").getSymbol();
		return new BodyParameter(bodySymbol, true);
	}

	@Override
	protected LispStruct internalApply(final Closure currentClosure) {
		return LispStructFactory.toCharacter(97);
	}

	@Override
	protected LispStruct getInitForm(final Closure currentClosure, final SymbolStructImpl symbolBinding) {

		final PackageStruct pkg1 = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl symbol1 = pkg1.findSymbol("SYMBOL1").getSymbol();
		if (symbolBinding.equals(symbol1)) {
			return LispStructFactory.toCharacter(100);
		}

		final PackageStruct pkg2 = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl symbol2 = pkg2.findSymbol("SYMBOL2").getSymbol();
		if (symbolBinding.equals(symbol2)) {
			return LispStructFactory.toCharacter(200);
		}

		return NILStruct.INSTANCE;
	}
}
