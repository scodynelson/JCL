/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import java.net.URL;
import javax.help.HelpBroker;
import javax.help.HelpSet;

import jcl.compiler.classloaders.CompilerClassLoader;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class Help extends BuiltInFunctionStructImpl {

	private static final String SEARCH_TERM_ARGUMENT = "SEARCH-TERM";

	public Help() {
		super("Invokes Java Help.",
		      CommonLispSymbols.HELP.getName(),
		      Parameters.forFunction(CommonLispSymbols.HELP.getName())
		                .optionalParameter(SEARCH_TERM_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.HELP;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final String searchString;
		if (arguments.hasOptionalArgument(SEARCH_TERM_ARGUMENT)) {
			final StringStruct searchTerm = arguments.getOptionalArgument(SEARCH_TERM_ARGUMENT, StringStruct.class);
			if (searchTerm == null) {
				searchString = "index";
			} else {
				searchString = searchTerm.toJavaString();
			}
		} else {
			searchString = "index";
		}

		try {
			// TODO: Full path works. Relative does not. We might want to figure out a way to do this better anyways.
			final URL jarUrl = new URL("jar:file:/Volumes/Dev/repo/JCL/jcl-application/src/main/resources/HelpSystem.jar!/HelpSystemMain.hs");
			final HelpSet helpSet = new HelpSet(CompilerClassLoader.INSTANCE, jarUrl);
			final HelpBroker helpBroker = helpSet.createHelpBroker();
			helpBroker.setCurrentID(searchString);
			helpBroker.setViewDisplayed(true);
			helpBroker.setDisplayed(true);
		} catch (final Exception e) {
			log.error(e.getMessage(), e);
			throw new ProgramErrorException("Error Loading Help System: " + e);
		}

		return NILStruct.INSTANCE;
	}
}