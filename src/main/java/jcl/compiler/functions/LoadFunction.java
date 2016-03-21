/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import jcl.LispStruct;
import jcl.compiler.CompilerVariables;
import jcl.conditions.exceptions.FileErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
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
public final class LoadFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOAD";
	private static final String FILESPEC_ARGUMENT = "FILESPEC";

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

	public LoadFunction() {
		super("Loads the file named by filespec into the Lisp environment.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FILESPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.VERBOSE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.PRINT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD).withInitialValue(TStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {


		final LispStruct filespec = arguments.getRequiredArgument(FILESPEC_ARGUMENT);
		final boolean verbose;
		if (arguments.hasKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD)) {
			verbose = arguments.getKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD, BooleanStruct.class).booleanValue();
		} else {
			final BooleanStruct currentLoadVerbose = CompilerVariables.LOAD_VERBOSE.getVariableValue();
			verbose = currentLoadVerbose.booleanValue();
		}
		final boolean print;
		if (arguments.hasKeyArgument(CommonLispSymbols.PRINT_KEYWORD)) {
			print = arguments.getKeyArgument(CommonLispSymbols.PRINT_KEYWORD, BooleanStruct.class).booleanValue();
		} else {
			final BooleanStruct currentLoadPrint = CompilerVariables.LOAD_PRINT.getVariableValue();
			print = currentLoadPrint.booleanValue();
		}
		final boolean ifDoesNotExist = arguments.getKeyArgument(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, BooleanStruct.class).booleanValue();
		final LispStruct externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);
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
			throw new FileErrorException("Filespec provided to LOAD does not exist: " + filespecPath, filespecFileStream);
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
				throw new FileErrorException("Cannot LOAD file with unsupported extension: " + filespecPath, filespecFileStream);
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
