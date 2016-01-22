/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.CompilerVariables;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
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
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class LoadFunction extends FunctionStruct {

	public static final SymbolStruct LOAD = GlobalPackageStruct.COMMON_LISP.intern("LOAD").getSymbol();

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

	@Autowired
	private ConfigurableApplicationContext applicationContext;

	private LoadFunction() {
		super("Loads the file named by filespec into the Lisp environment.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOAD.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LOAD);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct filespecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FILESPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(filespecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final List<KeyParameter> keyBindings = new ArrayList<>(4);

		final SymbolStruct verboseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("VERBOSE").getSymbol();

		final SymbolStruct verboseSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("VERBOSE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter verboseSuppliedPBinding = new SuppliedPParameter(verboseSuppliedP);

		final KeyParameter verboseKeyBinding = new KeyParameter(verboseArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.VERBOSE_KEYWORD, verboseSuppliedPBinding);
		keyBindings.add(verboseKeyBinding);

		final SymbolStruct printArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PRINT").getSymbol();

		final SymbolStruct printSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("PRINT-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter printSuppliedPBinding = new SuppliedPParameter(printSuppliedP);

		final KeyParameter printKeyBinding = new KeyParameter(printArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.PRINT_KEYWORD, printSuppliedPBinding);
		keyBindings.add(printKeyBinding);

		final SymbolStruct ifDoesNotExistArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("IF-DOES-NOT-EXIST").getSymbol();

		final SymbolStruct ifDoesNotExistSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("IF-DOES-NOT-EXIST-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter ifDoesNotExistSuppliedPBinding = new SuppliedPParameter(ifDoesNotExistSuppliedP);

		final KeyParameter ifDoesNotExistKeyBinding = new KeyParameter(ifDoesNotExistArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, ifDoesNotExistSuppliedPBinding);
		keyBindings.add(ifDoesNotExistKeyBinding);

		final SymbolStruct externalFormatArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("EXTERNAL-FORMAT").getSymbol();

		final SymbolStruct externalFormatSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("EXTERNAL-FORMAT-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter externalFormatSuppliedPBinding = new SuppliedPParameter(externalFormatSuppliedP);

		final KeyParameter externalFormatKeyBinding = new KeyParameter(externalFormatArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, externalFormatSuppliedPBinding);
		keyBindings.add(externalFormatKeyBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .keyBindings(keyBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct filespec = lispStructs[0];

		final BooleanStruct currentLoadVerbose = CompilerVariables.LOAD_VERBOSE.getVariableValue();
		final BooleanStruct currentLoadPrint = CompilerVariables.LOAD_PRINT.getVariableValue();

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
			final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
			final PathnameVersion nilVersion = new PathnameVersion(PathnameVersionComponentType.NIL);
			filespecPathname = mergePathnamesFunction.mergePathnames(filespec, defaultPathspec, nilVersion);
			final File pathnameFile = new File(filespecPathname.getNamestring());
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

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

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

	private LispStruct loadCompiledCode(final Path filespecPath, final boolean verbose, final boolean print) {

		try {
			final LoaderClassLoader cl = new LoaderClassLoader(filespecPath, verbose, print);
			final Class<?> classLoaded = cl.loadMainClass();

			if (classLoaded == null) {
				return NILStruct.INSTANCE;
			} else {
				final BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(classLoaded);
				final DefaultListableBeanFactory factory = (DefaultListableBeanFactory) applicationContext.getBeanFactory();

				final String beanName = classLoaded.getSimpleName();
				final BeanDefinition beanDefinition = builder.getBeanDefinition();
				factory.registerBeanDefinition(beanName, beanDefinition);

				final FunctionStruct function = (FunctionStruct) applicationContext.getBean(classLoaded);
				return function.apply();
			}
		} catch (final FileErrorException fee) {
			LOGGER.error(fee.getMessage(), fee.getCause());
			return NILStruct.INSTANCE;
		} catch (BeansException | IllegalStateException ex) {
			LOGGER.error("Error loading main definition for compiled file: '{}'", filespecPath, ex);
			return NILStruct.INSTANCE;
		}
	}
}
