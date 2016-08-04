/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.function.Closure;
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@SuppressWarnings("all")
@Component
public class TestLambdaGenerator extends CompiledFunctionStruct {

	public TestLambdaGenerator() {
		this(null);
	}

	protected TestLambdaGenerator(final Closure closure) {
		super("DocumentationString", closure);
		initLambdaListBindings();
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final PackageStruct pkg = PackageStruct.findPackage("SYSTEM");
		final SymbolStructImpl requiredSymbol = pkg.intern("REQUIRED-SYMBOL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(requiredSymbol, true);
		requiredBindings.add(requiredBinding);

		return requiredBindings;
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
	protected LispStruct internalApply(final Closure currentClosure) {
//		result = CharacterStruct.valueOf(97);
		return new TestGroundLambdaFunction(currentClosure);
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
