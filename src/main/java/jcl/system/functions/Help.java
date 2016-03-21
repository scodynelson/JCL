/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.net.MalformedURLException;
import java.net.URL;
import javax.help.BadIDException;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.HelpSetException;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.NILStruct;
import jcl.system.classloaders.CompilerClassLoader;
import org.springframework.stereotype.Component;

@Component
public final class Help extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "HELP";
	private static final String SEARCH_TERM_ARGUMENT = "SEARCH-TERM";

	public Help() {
		super("Invokes Java Help.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(SEARCH_TERM_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final String searchString;
		if (arguments.hasOptionalArgument(SEARCH_TERM_ARGUMENT)) {
			final StringStruct searchTerm = arguments.getOptionalArgument(SEARCH_TERM_ARGUMENT, StringStruct.class);
			searchString = searchTerm.getAsJavaString();
		} else {
			searchString = "index";
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