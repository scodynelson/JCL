/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.functions.pathname.MergePathnamesFunction;
import jcl.functions.pathname.TranslateLogicalPathnameFunction;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.pathname.LogicalPathnameStructImpl;
import jcl.lang.pathname.PathnameStructImpl;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.PathnameVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFilePathnameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COMPILE-FILE-PATHNAME";
	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	@Autowired
	private TranslateLogicalPathnameFunction translateLogicalPathnameFunction;

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

	public PathnameStructImpl compileFilePathname(final LispStruct inputFile, final LispStruct outputFile) {
		// NOTE: 'outputFile' will be null if it is not supplied.

		final PathnameStructImpl defaultPathnameDefaults = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
		final PathnameStructImpl mergedInputFile = mergePathnamesFunction.mergePathnames(inputFile, defaultPathnameDefaults);

		final PathnameType outputPathnameType = new PathnameType("jar");

		final boolean isLogicalInputFile = mergedInputFile instanceof LogicalPathnameStructImpl;

		if ((outputFile == null) && isLogicalInputFile) {
			final PathnameStructImpl translatedMergedInputFile = translateLogicalPathnameFunction.translateLogicalPathname(mergedInputFile);
			return LispStructFactory.toLogicalPathname(
					translatedMergedInputFile.getPathnameHost(),
					translatedMergedInputFile.getPathnameDirectory(),
					translatedMergedInputFile.getPathnameName(),
					outputPathnameType,
					translatedMergedInputFile.getPathnameVersion()
			);
		} else if (isLogicalInputFile) {
			final PathnameStructImpl translatedMergedInputFile = translateLogicalPathnameFunction.translateLogicalPathname(mergedInputFile);
			final PathnameStructImpl mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFile, translatedMergedInputFile);
			return LispStructFactory.toPathname(
					mergedOutputFile.getPathnameHost(),
					mergedOutputFile.getPathnameDevice(),
					mergedOutputFile.getPathnameDirectory(),
					mergedOutputFile.getPathnameName(),
					outputPathnameType,
					mergedOutputFile.getPathnameVersion()
			);
		} else if (outputFile == null) {
			return LispStructFactory.toPathname(
					mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion()
			);
		} else {
			final PathnameStructImpl mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFile, mergedInputFile);
			return LispStructFactory.toPathname(
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
