/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.pathname.LogicalPathnameStruct;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.stream.SynonymStreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TranslateLogicalPathnameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "TRANSLATE-LOGICAL-PATHNAME";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public TranslateLogicalPathnameFunction() {
		super("Translates pathname to a physical pathname, which it returns.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		return translateLogicalPathname(pathspec);
	}

	public PathnameStruct translateLogicalPathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		if (pathnameDesignator instanceof LogicalPathnameStruct) {
			final LogicalPathnameStruct logicalPathname = (LogicalPathnameStruct) pathnameDesignator;
			pathname = logicalPathname.translateLogicalPathname();
		} else if (pathnameDesignator instanceof SynonymStreamStruct) {
			final SynonymStreamStruct synonymStream = (SynonymStreamStruct) pathnameDesignator;
			final SymbolStruct streamSymbol = synonymStream.getSymbol();
			pathname = translateLogicalPathname(streamSymbol.getValue());
		} else {
			pathname = pathnameFunction.pathname(pathnameDesignator);
		}

		return pathname;
	}
}