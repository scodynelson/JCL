/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import javax.help.BadIDException;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.HelpSetException;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.AbstractExtensionsFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.system.classloaders.CompilerClassLoader;
import org.springframework.stereotype.Component;

@Component
public final class Help extends AbstractExtensionsFunctionStruct {

	public Help() {
		super("Invokes Java Help.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.EXTENSIONS, "SEARCH-TERM")
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

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

	@Override
	protected String functionName() {
		return "HELP";
	}
}