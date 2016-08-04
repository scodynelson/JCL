/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import java.util.ArrayList;
import java.util.List;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameDirectoryComponent;
import jcl.lang.pathname.PathnameDirectoryLevel;
import jcl.lang.pathname.PathnameDirectoryLevelType;
import jcl.lang.pathname.PathnameDirectoryType;
import jcl.lang.pathname.PathnameStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameDirectoryFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME-DIRECTORY";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public PathnameDirectoryFunction() {
		super("Returns the pathname-directory component of the pathname denoted by pathspec.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.CASE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		final PathnameStruct pathname = pathnameFunction.pathname(pathspec);
		final PathnameDirectory pathnameDirectory = pathname.getPathnameDirectory();
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
						directoryLevelValue = LispStructFactory.toString(directoryLevelString);
						break;
				}

				directoryList.add(directoryLevelValue);
			}

			returnValue = LispStructFactory.toProperList(directoryList);
		}

		return returnValue;
	}
}
