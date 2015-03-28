/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameStruct;
import jcl.printer.Printer;
import jcl.streams.StreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TranslateLogicPathnameFunction extends FunctionStruct {

	private static final long serialVersionUID = -4200334862391198062L;

	@Autowired
	private Printer printer;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO
		return null;
	}

	public PathnameStruct translateLogicalPathname(final LispStruct pathnameDesignator) {

		final PathnameStruct pathname;
		if (pathnameDesignator instanceof LogicalPathnameStruct) {
			pathname = (LogicalPathnameStruct) pathnameDesignator;
		} else if (pathnameDesignator instanceof PathnameStruct) {
			pathname = (PathnameStruct) pathnameDesignator;
		} else if (pathnameDesignator instanceof StringStruct) {
			final StringStruct namestring = (StringStruct) pathnameDesignator;
			pathname = PathnameStruct.buildPathname(namestring);
		} else if (pathnameDesignator instanceof StreamStruct) {
			final StreamStruct stream = (StreamStruct) pathnameDesignator;
			// TODO
//			final Path path = fileStream.getPath();
//			pathname = PathnameStruct.buildPathname(path);
			pathname = null;
		} else {
			final String printedObject = printer.print(pathnameDesignator);
			throw new TypeErrorException("Illegal type argument provided to PATHNAME-TYPE: " + printedObject);
		}

		return pathname;
	}
}
