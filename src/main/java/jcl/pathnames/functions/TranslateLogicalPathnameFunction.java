/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameFileStruct;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.StreamStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TranslateLogicalPathnameFunction extends FunctionStruct {

	public static final SymbolStruct<?> TRANSLATE_LOGICAL_PATHNAME = new SymbolStruct<>("TRANSLATE-LOGICAL-PATHNAME", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -4200334862391198062L;

	@Autowired
	private Printer printer;

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
		final ParameterAllocation pathspecArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol, pathspecArgAllocation);
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

			final Path path = logicalPathname.getPath();
			pathname = new PathnameFileStruct(path);
		} else if (pathnameDesignator instanceof SynonymStreamStruct) {
			final SynonymStreamStruct synonymStream = (SynonymStreamStruct) pathnameDesignator;
			final SymbolStruct<StreamStruct> streamSymbol = synonymStream.getSymbol();
			pathname = translateLogicalPathname(streamSymbol.getValue());
		} else {
			pathname = pathnameFunction.pathname(pathnameDesignator);
		}

		return pathname;
	}
}
