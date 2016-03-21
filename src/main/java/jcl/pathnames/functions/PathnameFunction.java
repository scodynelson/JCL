/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.io.File;
import java.nio.file.Path;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.FileStreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private Printer printer;

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
			final String printedObject = printer.print(pathnameDesignator);
			throw new TypeErrorException("Illegal pathname designator argument provided: " + printedObject);
		}

		return pathname;
	}
}
