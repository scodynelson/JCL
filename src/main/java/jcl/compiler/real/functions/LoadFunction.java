/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.conditions.exceptions.FileErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameVariables;
import jcl.pathnames.PathnameVersion;
import jcl.pathnames.PathnameVersionComponentType;
import jcl.pathnames.functions.MergePathnamesFunction;
import jcl.printer.Printer;
import jcl.reader.functions.ReadFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.FileStreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import jcl.system.classloaders.LoaderClassLoader;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LoadFunction extends FunctionStruct {

	public static final SymbolStruct<?> LOAD = new SymbolStruct<>("LOAD", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 348551085956831789L;

	private static final Logger LOGGER = LoggerFactory.getLogger(LoadFunction.class);

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Printer printer;

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	private LoadFunction() {
		super("Loads the file named by filespec into the Lisp environment.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOAD.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> filespecArgSymbol = new SymbolStruct<>("FILESPEC", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation filespecArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(filespecArgSymbol, filespecArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = new ArrayList<>();

		final SymbolStruct<?> verboseArgSymbol = new SymbolStruct<>("VERBOSE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation verboseArgAllocation = new ParameterAllocation(1);

		final SymbolStruct<?> verboseSuppliedP = new SymbolStruct<>("VERBOSE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation verboseSuppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding verboseSuppliedPBinding = new SuppliedPBinding(verboseSuppliedP, verboseSuppliedPAllocation);

		final KeyBinding verboseKeyBinding = new KeyBinding(verboseArgSymbol, verboseArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.VERBOSE_KEYWORD, verboseSuppliedPBinding);
		keyBindings.add(verboseKeyBinding);

		final SymbolStruct<?> printArgSymbol = new SymbolStruct<>("PRINT", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation printArgAllocation = new ParameterAllocation(3);

		final SymbolStruct<?> printSuppliedP = new SymbolStruct<>("PRINT-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation printSuppliedPAllocation = new ParameterAllocation(4);
		final SuppliedPBinding printSuppliedPBinding = new SuppliedPBinding(printSuppliedP, printSuppliedPAllocation);

		final KeyBinding printKeyBinding = new KeyBinding(printArgSymbol, printArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.PRINT_KEYWORD, printSuppliedPBinding);
		keyBindings.add(printKeyBinding);

		final SymbolStruct<?> ifDoesNotExistArgSymbol = new SymbolStruct<>("IF-DOES-NOT-EXIST", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation ifDoesNotExistArgAllocation = new ParameterAllocation(5);

		final SymbolStruct<?> ifDoesNotExistSuppliedP = new SymbolStruct<>("IF-DOES-NOT-EXIST-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation ifDoesNotExistSuppliedPAllocation = new ParameterAllocation(6);
		final SuppliedPBinding ifDoesNotExisteSuppliedPBinding = new SuppliedPBinding(ifDoesNotExistSuppliedP, ifDoesNotExistSuppliedPAllocation);

		final KeyBinding ifDoesNotExistKeyBinding = new KeyBinding(ifDoesNotExistArgSymbol, ifDoesNotExistArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, ifDoesNotExisteSuppliedPBinding);
		keyBindings.add(ifDoesNotExistKeyBinding);

		final SymbolStruct<?> externalFormatArgSymbol = new SymbolStruct<>("EXTERNAL-FORMAT", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation externalFormatArgAllocation = new ParameterAllocation(7);

		final SymbolStruct<?> externalFormatSuppliedP = new SymbolStruct<>("EXTERNAL-FORMAT-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation externalFormatSuppliedPAllocation = new ParameterAllocation(8);
		final SuppliedPBinding externalFormatSuppliedPBinding = new SuppliedPBinding(externalFormatSuppliedP, externalFormatSuppliedPAllocation);

		final KeyBinding externalFormatKeyBinding = new KeyBinding(externalFormatArgSymbol, externalFormatArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, externalFormatSuppliedPBinding);
		keyBindings.add(externalFormatKeyBinding);

		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct filespec = lispStructs[0];

		final BooleanStruct currentLoadVerbose = CompilerVariables.LOAD_VERBOSE.getValue();
		final BooleanStruct currentLoadPrint = CompilerVariables.LOAD_PRINT.getValue();

		boolean verbose = currentLoadVerbose.booleanValue();
		boolean print = currentLoadPrint.booleanValue();
		boolean ifDoesNotExist = true;

		final int length = lispStructs.length;
		if (length >= 3) {
			// 1 keyword
			final LispStruct firstKeyword = lispStructs[1];
			if (CommonLispSymbols.VERBOSE_KEYWORD.equals(firstKeyword)) {
				verbose = ((BooleanStruct) lispStructs[2]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(firstKeyword)) {
				print = ((BooleanStruct) lispStructs[2]).booleanValue();
			} else if (CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD.equals(firstKeyword)) {
				ifDoesNotExist = ((BooleanStruct) lispStructs[2]).booleanValue();
			}
		}
		if (length >= 5) {
			// 2 keywords
			final LispStruct secondKeyword = lispStructs[3];
			if (CommonLispSymbols.VERBOSE_KEYWORD.equals(secondKeyword)) {
				verbose = ((BooleanStruct) lispStructs[4]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(secondKeyword)) {
				print = ((BooleanStruct) lispStructs[4]).booleanValue();
			} else if (CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD.equals(secondKeyword)) {
				ifDoesNotExist = ((BooleanStruct) lispStructs[4]).booleanValue();
			}
		}
		if (length >= 7) {
			// 3 keywords
			final LispStruct thirdKeyword = lispStructs[5];
			if (CommonLispSymbols.VERBOSE_KEYWORD.equals(thirdKeyword)) {
				verbose = ((BooleanStruct) lispStructs[6]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(thirdKeyword)) {
				print = ((BooleanStruct) lispStructs[6]).booleanValue();
			} else if (CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD.equals(thirdKeyword)) {
				ifDoesNotExist = ((BooleanStruct) lispStructs[6]).booleanValue();
			}
		}
		if (length >= 9) {
			// 4 keywords
			final LispStruct fourthKeyword = lispStructs[7];
			if (CommonLispSymbols.VERBOSE_KEYWORD.equals(fourthKeyword)) {
				verbose = ((BooleanStruct) lispStructs[8]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(fourthKeyword)) {
				print = ((BooleanStruct) lispStructs[8]).booleanValue();
			} else if (CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD.equals(fourthKeyword)) {
				ifDoesNotExist = ((BooleanStruct) lispStructs[8]).booleanValue();
			}
		}
		return load(filespec, verbose, print, ifDoesNotExist);
	}

	public LispStruct load(final LispStruct filespec, final boolean verbose, final boolean print, final boolean ifDoesNotExist) {

		FileStreamStruct filespecFileStream = null;

		final Path filespecPath;
		final PathnameStruct filespecPathname;

		// NOTE: optimizations if the filespec is already a FileStreamStruct
		if (filespec instanceof FileStreamStruct) {
			filespecFileStream = (FileStreamStruct) filespec;
			filespecPath = filespecFileStream.getPath();
			filespecPathname = new PathnameStruct(filespecPath);
		} else {
			final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getValue();
			final PathnameVersion nilVersion = new PathnameVersion(PathnameVersionComponentType.NIL);
			filespecPathname = mergePathnamesFunction.mergePathnames(filespec, defaultPathspec, nilVersion);
			final URI pathnameURI = filespecPathname.getUri();
			final File pathnameFile = new File(pathnameURI.toString());
			filespecPath = pathnameFile.toPath();
		}

		final boolean filespecNotExists = Files.notExists(filespecPath);
		if (filespecNotExists && ifDoesNotExist) {
			throw new FileErrorException("Filespec provided to LOAD does not exist: " + filespecPath);
		}
		if (filespecNotExists) {
			return NILStruct.INSTANCE;
		}

		final LispStruct previousLoadPathname = CompilerVariables.LOAD_PATHNAME.getValue();
		final LispStruct previousLoadTruename = CompilerVariables.LOAD_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(filespecPathname);
		final Path filespecAbsolutePath = filespecPath.toAbsolutePath();
		final PathnameStruct filespecTruename = new PathnameStruct(filespecAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(filespecTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getValue();

		try {
			final String filespecNamestring = filespecPath.toString();
			if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lar") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".jar")) {
				return loadCompiledCode(filespecPath, verbose, print);
			} else if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lsp") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".lisp")) {
				if (filespecFileStream == null) {
					filespecFileStream = new FileStreamStruct(filespecPath);
				}
				return loadSourceCode(filespecFileStream, filespecPath, verbose, print);
			} else {
				throw new FileErrorException("Cannot LOAD file with unsupported extension: " + filespecPath);
			}
		} finally {
			CompilerVariables.LOAD_TRUENAME.setValue(previousLoadTruename);
			CompilerVariables.LOAD_PATHNAME.setValue(previousLoadPathname);

			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READTABLE.setValue(previousReadtable);
		}
	}

	private LispStruct loadSourceCode(final FileStreamStruct filespecFileStream, final Path filespecPath,
	                                  final boolean verbose, final boolean print) {

		if (verbose) {
			LOGGER.info("; Loading '{}'", filespecPath);
		}

		LispStruct form;
		do {
			form = readFunction.read(filespecFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);
			if (form == null) {
				continue;
			}

			final LispStruct evaluatedForm = evalFunction.eval(form);
			if (print) {
				final String printedObject = printer.print(evaluatedForm);
				LOGGER.info("; {}", printedObject);
			}
		} while (form != null);

		return TStruct.INSTANCE;
	}

	private static LispStruct loadCompiledCode(final Path filespecPath, final boolean verbose, final boolean print) {

		try {
			final LoaderClassLoader cl = new LoaderClassLoader(filespecPath, verbose, print);
			final Class<?> classLoaded = cl.loadMainClass();

			if (classLoaded == null) {
				return NILStruct.INSTANCE;
			} else {
				final Constructor<?> constructor = classLoaded.getConstructor();
				final FunctionStruct function = (FunctionStruct) constructor.newInstance();
				return function.apply();
			}
		} catch (InstantiationException | NoSuchMethodException | InvocationTargetException | IllegalAccessException ex) {
			LOGGER.error("Error loading main definition for compiled file: '{}'", filespecPath, ex);
			return NILStruct.INSTANCE;
		} catch (final FileErrorException fee) {
			LOGGER.error(fee.getMessage(), fee.getCause());
			return NILStruct.INSTANCE;
		}
	}
}
