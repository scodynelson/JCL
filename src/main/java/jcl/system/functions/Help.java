/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;
import javax.help.BadIDException;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.HelpSetException;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.classloaders.CompilerClassLoader;
import org.springframework.stereotype.Component;

@Component
public final class Help extends FunctionStruct {

	public static final SymbolStruct<?> HELP = new SymbolStruct<>("HELP", GlobalPackageStruct.EXTENSIONS);

	private static final long serialVersionUID = -2903697700427964980L;

	private Help() {
		super("Invokes Java Help.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		HELP.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();

		final SymbolStruct<?> searchTermArgSymbol = new SymbolStruct<>("SEARCH-TERM", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation searchTermArgAllocation = new ParameterAllocation(0);

		final SymbolStruct<?> searchTermSuppliedPSymbol = new SymbolStruct<>("SEARCH-TERM-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation searchTermSuppliedPAllocation = new ParameterAllocation(1);
		final SuppliedPBinding searchTermSuppliedPBinding = new SuppliedPBinding(searchTermSuppliedPSymbol, searchTermSuppliedPAllocation);

		final OptionalBinding defaultPathnameOptionalBinding = new OptionalBinding(searchTermArgSymbol, searchTermArgAllocation, NullStruct.INSTANCE, searchTermSuppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(defaultPathnameOptionalBinding);

		final RestBinding restBinding = null;
		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final String searchString;
		if (lispStructs.length == 0) {
			searchString = "index";
		} else {
			final StringStruct searchTerm = (StringStruct) lispStructs[0];
			searchString = searchTerm.getAsJavaString();
		}

		try {
			// TODO: Full path works. Relative does not. We might want to figure out a way to do this better anyways.
			final HelpSet helpSet = new HelpSet(CompilerClassLoader.INSTANCE, new URL("jar:file:../../resources/HelpSystem.jar!/HelpSystemMain.hs"));
			final HelpBroker helpBroker = helpSet.createHelpBroker();
			helpBroker.setCurrentID(searchString);
			helpBroker.setViewDisplayed(true);
			helpBroker.setDisplayed(true);
		} catch (BadIDException | javax.help.UnsupportedOperationException | HelpSetException | MalformedURLException e) {
			throw new ProgramErrorException("Error Loading Help System: " + e);
		}

		return NILStruct.INSTANCE;
	}
}