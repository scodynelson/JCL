/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.lang.LispStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.PathnameVariables;
import org.springframework.stereotype.Component;

@Component
public final class CompileFilePathnameFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COMPILE-FILE-PATHNAME";
	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	public CompileFilePathnameFunction() {
		super("Returns the pathname that compile-file would write into, if given the same arguments.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_FILE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.OUTPUT_FILE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .allowOtherKeys()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct inputFile = arguments.getRequiredArgument(INPUT_FILE_ARGUMENT);
		if (arguments.hasKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD)) {
			final LispStruct outputFile = arguments.getKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD);
			return compileFilePathname(inputFile, outputFile);
		} else {
			return compileFilePathname(inputFile, null);
		}
	}

	public PathnameStruct compileFilePathname(final LispStruct inputFile, final LispStruct outputFile) {
		// NOTE: 'outputFile' will be null if it is not supplied.

		final PathnameStruct inputFilePathname = PathnameStruct.toPathname(inputFile);
		final PathnameStruct defaultPathnameDefaults = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
		final PathnameStruct mergedInputFile = PathnameStruct.mergePathnames(inputFilePathname, defaultPathnameDefaults);

		final PathnameType outputPathnameType = new PathnameType("jar");

		final boolean isLogicalInputFile = mergedInputFile instanceof LogicalPathnameStruct;

		if ((outputFile == null) && isLogicalInputFile) {
			final PathnameStruct translatedMergedInputFile = PathnameStruct.translateLogicalPathname(mergedInputFile);
			return LogicalPathnameStruct.toLogicalPathname(
					translatedMergedInputFile.getPathnameHost(),
					translatedMergedInputFile.getPathnameDirectory(),
					translatedMergedInputFile.getPathnameName(),
					outputPathnameType,
					translatedMergedInputFile.getPathnameVersion()
			);
		} else if (isLogicalInputFile) {
			final PathnameStruct outputFilePathname = PathnameStruct.toPathname(outputFile);
			final PathnameStruct translatedMergedInputFile = PathnameStruct.translateLogicalPathname(mergedInputFile);
			final PathnameStruct mergedOutputFile = PathnameStruct.mergePathnames(outputFilePathname, translatedMergedInputFile);
			return PathnameStruct.toPathname(
					mergedOutputFile.getPathnameHost(),
					mergedOutputFile.getPathnameDevice(),
					mergedOutputFile.getPathnameDirectory(),
					mergedOutputFile.getPathnameName(),
					outputPathnameType,
					mergedOutputFile.getPathnameVersion()
			);
		} else if (outputFile == null) {
			return PathnameStruct.toPathname(
					mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion()
			);
		} else {
			final PathnameStruct outputFilePathname = PathnameStruct.toPathname(outputFile);
			final PathnameStruct mergedOutputFile = PathnameStruct.mergePathnames(outputFilePathname, mergedInputFile);
			return PathnameStruct.toPathname(
					mergedOutputFile.getPathnameHost(),
					mergedOutputFile.getPathnameDevice(),
					mergedOutputFile.getPathnameDirectory(),
					mergedOutputFile.getPathnameName(),
					outputPathnameType,
					mergedOutputFile.getPathnameVersion()
			);
		}
	}
}
