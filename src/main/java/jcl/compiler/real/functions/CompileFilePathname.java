/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameStructType;
import jcl.pathnames.PathnameType;
import jcl.pathnames.PathnameVariables;
import jcl.pathnames.functions.MergePathnamesFunction;
import jcl.pathnames.functions.PathnameTypeFunction;
import jcl.pathnames.functions.TranslateLogicPathnameFunction;
import jcl.printer.Printer;
import jcl.streams.FileStreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CompileFilePathname extends FunctionStruct {

	private static final long serialVersionUID = 591587108565227067L;

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	@Autowired
	private TranslateLogicPathnameFunction translateLogicPathnameFunction;

	@Autowired
	private PathnameTypeFunction pathnameTypeFunction;

	@Autowired
	private Printer printer;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO
		return null;
	}

	public PathnameStruct compileFilePathname(final LispStruct inputFile, final LispStruct outputFile) {

		if (!(inputFile instanceof PathnameStruct) &&
				!(inputFile instanceof StringStruct) &&
				!(inputFile instanceof FileStreamStruct)) {
			final String printedObject = printer.print(inputFile);
			throw new TypeErrorException("Illegal type argument provided to COMPILE-FILE-PATHNAME: " + printedObject);
		}

		// TODO: Move '.jar' into property file
		final PathnameType pathnameType = new PathnameType(".jar");

		final PathnameStruct defaultPathnameDefaults = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue();

		final PathnameStruct mergedInputFile = mergePathnamesFunction.mergePathnames(inputFile, defaultPathnameDefaults);

		final boolean outputFileNotSupplied;
		final PathnameType outputPathnameType;

		if (NullStruct.INSTANCE.equals(outputFile)) {
			outputFileNotSupplied = true;
			outputPathnameType = pathnameType;
		} else {
			outputFileNotSupplied = false;
			outputPathnameType = pathnameTypeFunction.pathnameType(outputFile);
		}

		final boolean isLogicalInputFile = mergedInputFile instanceof LogicalPathnameStruct;

		if (outputFileNotSupplied && isLogicalInputFile) {
			// TODO: use the :COMMON Pathname Case. However, the pathnames don't support the :LOCAL or :COMMON casing right now. :(
			return PathnameStruct.buildPathname(mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion(),
					PathnameStructType.FILE);
		} else if (isLogicalInputFile) {
			final PathnameStruct translatedMergedInputFile = translateLogicPathnameFunction.translateLogicalPathname(mergedInputFile);
			return PathnameStruct.buildPathname(mergedInputFile.getPathnameHost(),
					translatedMergedInputFile.getPathnameDevice(),
					translatedMergedInputFile.getPathnameDirectory(),
					translatedMergedInputFile.getPathnameName(),
					outputPathnameType,
					translatedMergedInputFile.getPathnameVersion(),
					PathnameStructType.FILE);
		} else {
			return PathnameStruct.buildPathname(mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion(),
					PathnameStructType.FILE);
		}
	}
}
