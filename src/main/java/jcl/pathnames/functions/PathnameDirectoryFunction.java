/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameDirectory;
import jcl.pathnames.PathnameDirectoryComponent;
import jcl.pathnames.PathnameDirectoryLevel;
import jcl.pathnames.PathnameDirectoryLevelType;
import jcl.pathnames.PathnameDirectoryType;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameDirectoryFunction extends FunctionStruct {

	public static final SymbolStruct PATHNAME_DIRECTORY = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME-DIRECTORY").getSymbol();

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameDirectoryFunction() {
		super("Returns the pathname-directory component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_DIRECTORY.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME_DIRECTORY);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct caseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE").getSymbol();

		final SymbolStruct caseSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE-P-").getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(caseSuppliedPSymbol);

		final KeyParameter keyBinding = new KeyParameter(caseArgSymbol, NILStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyParameter> keyBindings = Collections.singletonList(keyBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .keyBindings(keyBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final LispStruct pathspec = lispStructs[0];
		final PathnameDirectory pathnameDirectory = pathnameDirectory(pathspec);
		if (pathnameDirectory == null) {
			return NILStruct.INSTANCE;
		}

		final PathnameDirectoryComponent directoryComponent = pathnameDirectory.getDirectoryComponent();
		final LispStruct returnValue;

		if (directoryComponent == null) {
			final PathnameComponentType componentType = pathnameDirectory.getComponentType();
			returnValue = componentType.getValue();
		} else {
			final List<LispStruct> directoryList = new ArrayList<>();

			final PathnameDirectoryType pathnameDirectoryType = directoryComponent.getPathnameDirectoryType();
			directoryList.add(pathnameDirectoryType.getValue());

			final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();
			for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {

				final PathnameDirectoryLevelType directoryLevelType = directoryLevel.getDirectoryLevelType();

				LispStruct directoryLevelValue = null;
				switch (directoryLevelType) {
					case WILD:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case BACK:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case UP:
						directoryLevelValue = directoryLevelType.getValue();
						break;
					case NULL:
						final String directoryLevelString = directoryLevel.getDirectoryLevel();
						directoryLevelValue = new StringStruct(directoryLevelString);
						break;
				}

				directoryList.add(directoryLevelValue);
			}

			returnValue = ListStruct.buildProperList(directoryList);
		}

		return returnValue;
	}

	public PathnameDirectory pathnameDirectory(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameDirectory();
	}
}
