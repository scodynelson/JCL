/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameStruct;
import jcl.streams.StreamStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TranslateLogicalPathnameFunction extends FunctionStruct {

	public static final SymbolStruct<?> TRANSLATE_LOGICAL_PATHNAME = new SymbolStruct<>("TRANSLATE-LOGICAL-PATHNAME", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -4200334862391198062L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private TranslateLogicalPathnameFunction() {
		super("Translates pathname to a physical pathname, which it returns.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		TRANSLATE_LOGICAL_PATHNAME.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = new SymbolStruct<>("PATHSPEC", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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

			final URI uri = logicalPathname.getUri();
			pathname = new PathnameStruct(uri);
		} else if (pathnameDesignator instanceof SynonymStreamStruct) {
			final SynonymStreamStruct synonymStream = (SynonymStreamStruct) pathnameDesignator;
			final SymbolStruct<? extends StreamStruct> streamSymbol = synonymStream.getSymbol();
			pathname = translateLogicalPathname(streamSymbol.getValue());
		} else {
			pathname = pathnameFunction.pathname(pathnameDesignator);
		}

		return pathname;
	}
}
