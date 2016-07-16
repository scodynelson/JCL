/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import java.io.File;
import java.nio.file.Path;

import jcl.lang.LispStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.stream.FileStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class PathnameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	public PathnameFunction() {
		super("Returns the pathname denoted by pathspec.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		return pathname(pathspec);
	}

	public PathnameStruct pathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		final String namestring;
		if (pathnameDesignator instanceof PathnameStruct) {
			pathname = (PathnameStruct) pathnameDesignator;
		} else if (pathnameDesignator instanceof StringStruct) {
			final StringStruct namestringStruct = (StringStruct) pathnameDesignator;
			namestring = namestringStruct.getAsJavaString();
			pathname = new PathnameStruct(namestring);
		} else if (pathnameDesignator instanceof FileStreamStruct) {
			final FileStreamStruct fileStream = (FileStreamStruct) pathnameDesignator;
			final Path path = fileStream.getPath();
			final File file = path.toFile();
			namestring = file.getAbsolutePath();
			pathname = new PathnameStruct(namestring);
		} else {
			throw new TypeErrorException("Illegal pathname designator argument provided: " + pathnameDesignator);
		}

		return pathname;
	}
}
