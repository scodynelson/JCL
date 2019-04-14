package jcl.compiler.function;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import jcl.compiler.classloaders.LoaderClassLoader;
import jcl.lang.BooleanStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.pathname.PathnameVersionComponentType;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.PathnameVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

@Slf4j
@UtilityClass
public final class InternalLoad {

	public static LispStruct load(final LispStruct filespec, final BooleanStruct verboseVal, final BooleanStruct printVal,
	                              final BooleanStruct ifDoesNotExistVal, final LispStruct externalFormat) {
		final boolean verbose = verboseVal.toJavaPBoolean();
		final boolean print = printVal.toJavaPBoolean();
		final boolean ifDoesNotExist = ifDoesNotExistVal.toJavaPBoolean();

		FileStreamStruct filespecFileStream = null;

		final Path filespecPath;
		final PathnameStruct filespecPathname;

		// NOTE: optimizations if the filespec is already a FileStreamStruct
		if (filespec instanceof FileStreamStruct) {
			filespecFileStream = (FileStreamStruct) filespec;
			filespecPath = filespecFileStream.getPath();
			filespecPathname = PathnameStruct.toPathname(filespecPath);
		} else {
			final PathnameStruct filespecAsPathname = PathnameStruct.toPathname(filespec);
			final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
			final PathnameVersion nilVersion = new PathnameVersion(PathnameVersionComponentType.NIL);
			filespecPathname = PathnameStruct.mergePathnames(filespecAsPathname, defaultPathspec, nilVersion);
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
		final PathnameStruct filespecTruename = PathnameStruct.toPathname(filespecAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(filespecTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		try {
			final String filespecNamestring = filespecPath.toString();
			if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lar") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".jar")) {
				return loadCompiledCode(filespecPath, verbose, print);
			} else if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lsp") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".lisp")) {
				if (filespecFileStream == null) {
					filespecFileStream = FileStreamStruct.toFileStream(filespecPath);
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

	private static LispStruct loadSourceCode(final FileStreamStruct filespecFileStream, final Path filespecPath,
	                                         final boolean verbose, final boolean print) {

		if (verbose) {
			log.info("; Loading '{}'", filespecPath);
		}

		LispStruct form;
		do {
			form = InternalRead.read(filespecFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);
			if (form == null) {
				continue;
			}

			final LispStruct evaluatedForm = InternalEval.eval(form);
			if (print) {
				log.info("; {}", evaluatedForm);
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
				final FunctionStruct function = (FunctionStruct) classLoaded.getDeclaredConstructor().newInstance();
				function.afterPropertiesSet();
				return function.apply();
			}
		} catch (final FileErrorException fee) {
			log.error(fee.getMessage(), fee.getCause());
			return NILStruct.INSTANCE;
		} catch (Exception ex) {
			log.error("Error loading main definition for compiled file: '{}'", filespecPath, ex);
			return NILStruct.INSTANCE;
		}
	}
}
