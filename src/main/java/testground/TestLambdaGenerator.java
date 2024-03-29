/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings("all")
public class TestLambdaGenerator extends CompiledFunctionStruct {

	public TestLambdaGenerator() {
		this(null);
	}

	protected TestLambdaGenerator(final Environment environment) {
		super("DocumentationString", environment);
		initLambdaListBindings(environment);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings(final Environment currentEnvironment) {
		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct requiredSymbol = pkg.intern("REQUIRED-SYMBOL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(requiredSymbol, true);
		requiredBindings.add(requiredBinding);

		return requiredBindings;
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings(final Environment currentEnvironment) {
		final List<OptionalParameter> optionalBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct optionalSymbol = pkg.intern("OPTIONAL-SYMBOL").getSymbol();
		final LispStruct optionalInitForm = NILStruct.INSTANCE;

		pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct optionalSuppliedPSymbol = pkg.intern("OPTIONAL-SYMBOL-P").getSymbol();
		final SuppliedPParameter optionalSuppliedPBinding = new SuppliedPParameter(optionalSuppliedPSymbol, false);

		final OptionalParameter optionalBinding = new OptionalParameter(optionalSymbol, optionalInitForm, false, optionalSuppliedPBinding);
		optionalBindings.add(optionalBinding);

		return optionalBindings;
	}

	@Override
	protected RestParameter getRestBinding(final Environment currentEnvironment) {
		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct restSymbol = pkg.intern("REST-SYMBOL").getSymbol();
		return new RestParameter(restSymbol, true);
	}

	@Override
	protected List<KeyParameter> getKeyBindings(final Environment currentEnvironment) {
		final List<KeyParameter> keyBindings = new ArrayList<>();

		PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct keySymbol = pkg.intern("KEY-SYMBOL").getSymbol();
		final LispStruct keyInitForm = NILStruct.INSTANCE;

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
	protected boolean getAllowOtherKeys(final Environment currentEnvironment) {
		return true;
	}

	@Override
	protected List<AuxParameter> getAuxBindings(final Environment currentEnvironment) {
		final List<AuxParameter> auxBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct auxSymbol = pkg.intern("AUX-SYMBOL").getSymbol();
		final LispStruct auxInitForm = NILStruct.INSTANCE;

		final AuxParameter auxBinding = new AuxParameter(auxSymbol, auxInitForm, false);
		auxBindings.add(auxBinding);

		return auxBindings;
	}

	@Override
	protected LispStruct internalApply(final Environment currentEnvironment) {
//		result = CharacterStruct.valueOf(97);
		return new TestGroundLambdaFunction(currentEnvironment);
	}

	@Override
	protected LispStruct getInitForm(final Environment currentEnvironment, final SymbolStruct symbolBinding) {

		final PackageStruct pkg1 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol1 = pkg1.findSymbol("SYMBOL1").getSymbol();
		if (symbolBinding.eq(symbol1)) {
			return CharacterStruct.toLispCharacter(100);
		}

		final PackageStruct pkg2 = PackageStruct.findPackage("SYSTEM");
		final SymbolStruct symbol2 = pkg2.findSymbol("SYMBOL2").getSymbol();
		if (symbolBinding.eq(symbol2)) {
			return CharacterStruct.toLispCharacter(200);
		}

		return NILStruct.INSTANCE;
	}
}
