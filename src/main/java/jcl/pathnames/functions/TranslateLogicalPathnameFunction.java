/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TranslateLogicalPathnameFunction extends FunctionStruct {

	public static final SymbolStruct TRANSLATE_LOGICAL_PATHNAME = GlobalPackageStruct.COMMON_LISP.intern("TRANSLATE-LOGICAL-PATHNAME").getSymbol();

	@Autowired
	private PathnameFunction pathnameFunction;

	private TranslateLogicalPathnameFunction() {
		super("Translates pathname to a physical pathname, which it returns.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		TRANSLATE_LOGICAL_PATHNAME.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(TRANSLATE_LOGICAL_PATHNAME);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
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
