/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.LogicalPathnameStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.PathnameVariables;
import jcl.pathnames.functions.MergePathnamesFunction;
import jcl.pathnames.functions.TranslateLogicalPathnameFunction;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFilePathnameFunction extends FunctionStruct {

	public static final SymbolStruct<?> COMPILE_FILE_PATHNAME = GlobalPackageStruct.COMMON_LISP.intern("COMPILE-FILE-PATHNAME").getSymbol();

	private static final long serialVersionUID = 591587108565227067L;

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	@Autowired
	private TranslateLogicalPathnameFunction translateLogicalPathnameFunction;

	private CompileFilePathnameFunction() {
		super("Returns the pathname that compile-file would write into, if given the same arguments.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		COMPILE_FILE_PATHNAME.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(COMPILE_FILE_PATHNAME);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> inputFileArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INPUT-FILE").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(inputFileArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> outputFileArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OUTPUT-FILE").getSymbol();

		final SymbolStruct<?> outputSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("OUTPUT-FILE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(outputSuppliedPSymbol);

		final KeyParameter keyBinding = new KeyParameter(outputFileArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.OUTPUT_FILE_KEYWORD, suppliedPBinding);
		final List<KeyParameter> keyBindings = Collections.singletonList(keyBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .keyBindings(keyBindings)
		                                       .allowOtherKeys(true)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct inputFile = lispStructs[0];
		if (lispStructs.length > 2) {
			final LispStruct outputFile = lispStructs[2];
			return compileFilePathname(inputFile, outputFile);
		} else {
			return compileFilePathname(inputFile, null);
		}
	}

	public PathnameStruct compileFilePathname(final LispStruct inputFile, final LispStruct outputFile) {
		// NOTE: 'outputFile' will be null if it is not supplied.

		final PathnameStruct defaultPathnameDefaults = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue();
		final PathnameStruct mergedInputFile = mergePathnamesFunction.mergePathnames(inputFile, defaultPathnameDefaults);

		final PathnameType outputPathnameType = new PathnameType("jar");

		final boolean isLogicalInputFile = mergedInputFile instanceof LogicalPathnameStruct;

		if ((outputFile == null) && isLogicalInputFile) {
			final PathnameStruct translatedMergedInputFile = translateLogicalPathnameFunction.translateLogicalPathname(mergedInputFile);
			return new LogicalPathnameStruct(
					translatedMergedInputFile.getPathnameHost(),
					translatedMergedInputFile.getPathnameDirectory(),
					translatedMergedInputFile.getPathnameName(),
					outputPathnameType,
					translatedMergedInputFile.getPathnameVersion()
			);
		} else if (isLogicalInputFile) {
			final PathnameStruct translatedMergedInputFile = translateLogicalPathnameFunction.translateLogicalPathname(mergedInputFile);
			final PathnameStruct mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFile, translatedMergedInputFile);
			return new PathnameStruct(
					mergedOutputFile.getPathnameHost(),
					mergedOutputFile.getPathnameDevice(),
					mergedOutputFile.getPathnameDirectory(),
					mergedOutputFile.getPathnameName(),
					outputPathnameType,
					mergedOutputFile.getPathnameVersion()
			);
		} else if (outputFile == null) {
			return new PathnameStruct(
					mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion()
			);
		} else {
			final PathnameStruct mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFile, mergedInputFile);
			return new PathnameStruct(
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
