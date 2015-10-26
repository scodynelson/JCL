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
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
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

	public static final SymbolStruct<?> HELP = GlobalPackageStruct.EXTENSIONS.intern("HELP").getSymbol();

	private static final long serialVersionUID = -2903697700427964980L;

	private Help() {
		super("Invokes Java Help.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		HELP.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(HELP);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> searchTermArgSymbol = GlobalPackageStruct.EXTENSIONS.intern("SEARCH-TERM").getSymbol();

		final SymbolStruct<?> searchTermSuppliedPSymbol = GlobalPackageStruct.EXTENSIONS.intern("SEARCH-TERM-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter searchTermSuppliedPBinding = new SuppliedPParameter(searchTermSuppliedPSymbol);

		final OptionalParameter searchTermOptionalBinding = new OptionalParameter(searchTermArgSymbol, NullStruct.INSTANCE, searchTermSuppliedPBinding);
		final List<OptionalParameter> optionalBindings = Collections.singletonList(searchTermOptionalBinding);

		return new OrdinaryLambdaList.Builder().optionalBindings(optionalBindings)
		                                               .build();
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