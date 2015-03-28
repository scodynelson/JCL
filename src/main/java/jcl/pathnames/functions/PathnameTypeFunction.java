/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.nio.file.Path;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.printer.Printer;
import jcl.streams.FileStreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PathnameTypeFunction extends FunctionStruct {

	private static final long serialVersionUID = -4494745298812583275L;

	@Autowired
	private Printer printer;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO
		return null;
	}

	public PathnameType pathnameType(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		if (pathnameDesignator instanceof PathnameStruct) {
			pathname = (PathnameStruct) pathnameDesignator;
		} else if (pathnameDesignator instanceof StringStruct) {
			final StringStruct namestring = (StringStruct) pathnameDesignator;
			pathname = PathnameStruct.buildPathname(namestring);
		} else if (pathnameDesignator instanceof FileStreamStruct) {
			final FileStreamStruct fileStream = (FileStreamStruct) pathnameDesignator;
			final Path path = fileStream.getPath();
			pathname = PathnameStruct.buildPathname(path);
		} else {
			final String printedObject = printer.print(pathnameDesignator);
			throw new TypeErrorException("Illegal type argument provided to PATHNAME-TYPE: " + printedObject);
		}

		return pathname.getPathnameType();
	}
}
