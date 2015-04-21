/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameVersion;
import jcl.pathnames.PathnameVersionComponentType;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameVersionFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME_VERSION = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME-VERSION").getSymbol();

	private static final long serialVersionUID = -5346065974256023261L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameVersionFunction() {
		super("Returns the pathname-version component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_VERSION.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME_VERSION);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		final PathnameVersion pathnameVersion = pathnameVersion(pathspec);
		if (pathnameVersion == null) {
			return NullStruct.INSTANCE;
		}

		final Integer version = pathnameVersion.getVersion();
		final LispStruct returnValue;

		if (version == null) {
			final PathnameVersionComponentType componentType = pathnameVersion.getComponentType();
			returnValue = componentType.getValue();
		} else {
			final String versionString = version.toString();
			returnValue = new StringStruct(versionString);
		}

		return returnValue;
	}

	public PathnameVersion pathnameVersion(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameVersion();
	}
}
