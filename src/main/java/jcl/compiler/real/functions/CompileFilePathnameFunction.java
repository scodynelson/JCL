/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
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

	public static final SymbolStruct<?> COMPILE_FILE_PATHNAME = new SymbolStruct<>("COMPILE-FILE-PATHNAME", GlobalPackageStruct.COMMON_LISP);

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
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> inputFileArgSymbol = new SymbolStruct<>("INPUT-FILE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation inputFileArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(inputFileArgSymbol, inputFileArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final SymbolStruct<?> outputFileArgSymbol = new SymbolStruct<>("OUTPUT-FILE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation outputFileArgAllocation = new ParameterAllocation(1);

		final SymbolStruct<?> outputSuppliedPSymbol = new SymbolStruct<>("OUTPUT-FILE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation suppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(outputSuppliedPSymbol, suppliedPAllocation);

		final KeyBinding keyBinding = new KeyBinding(outputFileArgSymbol, outputFileArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyBinding> keyBindings = Collections.singletonList(keyBinding);

		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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
			return new PathnameStruct(
					mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion()
			);
		} else if (isLogicalInputFile) {
			final PathnameStruct outputFilePathname = new PathnameStruct(null, null, null, null, outputPathnameType, null);
			final PathnameStruct translatedMergedInputFile = translateLogicalPathnameFunction.translateLogicalPathname(mergedInputFile);
			final PathnameStruct mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFilePathname, translatedMergedInputFile);

			return new PathnameStruct(
					mergedOutputFile.getPathnameHost(),
					mergedOutputFile.getPathnameDevice(),
					mergedOutputFile.getPathnameDirectory(),
					mergedOutputFile.getPathnameName(),
					outputPathnameType,
					mergedOutputFile.getPathnameVersion()
			);
		} else {
			final PathnameStruct outputFilePathname = new PathnameStruct(null, null, null, null, outputPathnameType, null);
			final PathnameStruct mergedOutputFile = mergePathnamesFunction.mergePathnames(outputFilePathname, mergedInputFile);

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
