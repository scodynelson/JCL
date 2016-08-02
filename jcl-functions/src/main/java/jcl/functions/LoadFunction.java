/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import jcl.compiler.classloaders.LoaderClassLoader;
import jcl.compiler.functions.EvalFunction;
import jcl.functions.pathname.MergePathnamesFunction;
import jcl.functions.readtable.ReadFunction;
import jcl.lang.BooleanStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.FileStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.statics.PackageVariables;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.statics.PathnameVariables;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.pathname.PathnameVersionComponentType;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
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
			filespecPathname = LispStructFactory.toPathname(filespecPath);
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
		final PathnameStruct filespecTruename = LispStructFactory.toPathname(filespecAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(filespecTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		try {
			final String filespecNamestring = filespecPath.toString();
			if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lar") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".jar")) {
				return loadCompiledCode(filespecPath, verbose, print);
			} else if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lsp") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".lisp")) {
				if (filespecFileStream == null) {
					filespecFileStream = LispStructFactory.toFileStream(filespecPath);
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
				LOGGER.info("; {}", evaluatedForm);
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
