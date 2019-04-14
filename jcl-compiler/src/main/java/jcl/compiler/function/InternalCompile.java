package jcl.compiler.function;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Pattern;

import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.sa.SemanticAnalyzer;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.internal.DeclarationStructImpl;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.PathnameVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;

@Slf4j
@UtilityClass
public final class InternalCompile {

	private static final Pattern VALID_FILE_CLASS_NAME_PATTERN = Pattern.compile("[^a-zA-Z0-9]");

	public static LispStruct compile(final LispStruct name, final LispStruct uncompiledDefinition) {

		if ((uncompiledDefinition != null) && (uncompiledDefinition != NILStruct.INSTANCE)) {
			CompileResult compiledDefinition = null;

			final FunctionStruct function;
			if (uncompiledDefinition instanceof FunctionStruct) {
				function = (FunctionStruct) uncompiledDefinition;
			} else {
				compiledDefinition = CompileForm.compile(uncompiledDefinition);
				final FunctionStruct compiledDefinitionFunction = compiledDefinition.getFunction();
				final LispStruct compiledDefinitionResult = compiledDefinitionFunction.apply();

				if (!(compiledDefinitionResult instanceof FunctionStruct)) {
					throw new ProgramErrorException("Error compiling anonymous function : " + uncompiledDefinition + " is not a valid lambda expression.");
				}
				function = (FunctionStruct) compiledDefinitionResult;
			}

			if (name instanceof SymbolStruct) {
				final SymbolStruct nameSymbol = (SymbolStruct) name;
				nameSymbol.setFunction(function);
			} else if (!NILStruct.INSTANCE.eq(name)) {
				throw new ErrorException("The value " + name + " is not an acceptable function name.");
			}

			if (compiledDefinition == null) {
				return ValuesStruct.valueOf(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
			} else {
				return ValuesStruct.valueOf(
						function,
						BooleanStruct.toLispBoolean(compiledDefinition.isCompiledWithWarnings()),
						BooleanStruct.toLispBoolean(compiledDefinition.isFailedToCompile())
				);
			}
		}

		if (!(name instanceof SymbolStruct)) {
			throw new ErrorException("The value " + name + " is not an acceptable function name.");
		}
		final SymbolStruct nameSymbol = (SymbolStruct) name;

		final MacroFunctionExpanderInter macroFunction = nameSymbol.getMacroFunctionExpander();
		if (macroFunction != null) {
			return ValuesStruct.valueOf(macroFunction, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final boolean hasFunction = nameSymbol.hasFunction();
		if (hasFunction) {
			final FunctionStruct function = nameSymbol.getFunction();
			return ValuesStruct.valueOf(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		throw new ErrorException("No definition found for " + nameSymbol);
	}

	public static LispStruct compileFile(final LispStruct inputFile, final LispStruct outputFile,
	                                     final BooleanStruct verboseVal, final BooleanStruct printVal,
	                                     final LispStruct externalFormat) {
		// NOTE: 'outputFile' will be null if it is not supplied.
		final boolean verbose = verboseVal.toJavaPBoolean();
		final boolean print = printVal.toJavaPBoolean();

		final PathnameStruct inputFilePathname = PathnameStruct.toPathname(inputFile);
		final File inputFilePathnameFile = new File(inputFilePathname.getNamestring());
		final Path inputFilePath = inputFilePathnameFile.toPath();

		final boolean inputFileNotExists = Files.notExists(inputFilePath);
		if (inputFileNotExists) {
			// TODO: This should take a stream!!!
			throw new FileErrorException("Input file provided to COMPILE-FILE does not exist: " + inputFilePath, null);
		}

		final String inputFileNamestring = inputFilePath.toString();
		if (!StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lsp") && !StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lisp")) {
			// TODO: This should take a stream!!!
			throw new FileErrorException("Input file provided to COMPILE-FILE must have an extension of '.lsp' or '.lisp'", null);
		}

		final Instant startTime = Instant.now();
		if (verbose) {
			final String javaVersion = SystemUtils.JAVA_VERSION;
			log.info("; Java Compiler Version {}", javaVersion);

			final LocalDateTime now = LocalDateTime.now();
			log.info("; Compiling '{}' on {}", inputFileNamestring, now);
			log.info("");
		}

		final PathnameStruct outputFilePathname = InternalCompile.compileFilePathname(inputFilePathname, outputFile);
		final File outputFilePathnameFile = new File(outputFilePathname.getNamestring());
		final Path outputFilePath = outputFilePathnameFile.toPath();

		final LispStruct previousCompileFilePathname = CompilerVariables.COMPILE_FILE_PATHNAME.getValue();
		final LispStruct previousCompileFileTruename = CompilerVariables.COMPILE_FILE_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(outputFilePathname);
		final Path outputFileAbsolutePath = outputFilePath.toAbsolutePath();
		final PathnameStruct outputFileTruename = PathnameStruct.toPathname(outputFileAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(outputFileTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		boolean compiledSuccessfully = false;
		try {
			final FileStreamStruct inputFileStream = FileStreamStruct.toFileStream(inputFilePath);
			final List<LispStruct> forms = new ArrayList<>();

			LispStruct form;
			do {
				form = InternalRead.read(inputFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

				if (form instanceof ListStruct) {
					forms.add(form);
				} else if (form != null) {
					final Long currentFilePosition = inputFileStream.filePosition(null);
					// TODO: can we rework this to tell what line we're on???
					log.info("; Deleted a non-list form '{}' found at position {}.", form, currentFilePosition);
					form = NILStruct.INSTANCE;

					compiledWithWarnings = TStruct.INSTANCE;
				}
			} while (form != null);

			if (print && compiledWithWarnings.toJavaPBoolean()) {
				// If we printed warnings, make sure to print a newline afterwards.
				log.info("");
			}

			final String inputFileName = inputFilePath.getFileName().toString();
			String inputClassName = FilenameUtils.getBaseName(inputFileName);
			inputClassName = StringUtils.capitalize(inputClassName);
			inputClassName = VALID_FILE_CLASS_NAME_PATTERN.matcher(inputClassName).replaceAll("_");
			final ListStruct fileLambda = buildFileLambda(forms, inputClassName);

			final LambdaStruct analyzedFileLambda = SemanticAnalyzer.analyze(fileLambda);
			final Deque<JavaClassBuilder> javaClassBuilderDeque = IntermediateCodeGenerator.generate(analyzedFileLambda);

			writeToJar(javaClassBuilderDeque, outputFilePath, inputFileName, inputClassName, print);
			compiledSuccessfully = true;

			return ValuesStruct.valueOf(outputFileTruename, compiledWithWarnings, NILStruct.INSTANCE);
		} catch (final IOException e) {
			compiledSuccessfully = false;

			log.error("Error in COMPILE-FILE for file: '{}'", inputFilePath, e);

			return ValuesStruct.valueOf(NILStruct.INSTANCE, compiledWithWarnings, TStruct.INSTANCE);
		} finally {
			if (compiledSuccessfully && verbose) {
				log.info("\n; '{}' written", outputFilePath);

				final LocalDateTime now = LocalDateTime.now();
				log.info("; Compilation finished in {}.", now);
			} else if (verbose) {
				final Instant endTime = Instant.now();
				final Duration duration = Duration.between(startTime, endTime);
				log.info("; Compilation aborted after {}.", duration);
			}
			log.info("");

			CompilerVariables.COMPILE_FILE_TRUENAME.setValue(previousCompileFileTruename);
			CompilerVariables.COMPILE_FILE_PATHNAME.setValue(previousCompileFilePathname);

			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READTABLE.setValue(previousReadtable);
		}
	}

	private static ListStruct buildFileLambda(final List<LispStruct> forms, final String inputClassName) {
		final StringStruct newJavaClassName = StringStruct.toLispString("jcl." + inputClassName);
		final ListStruct javaClassNameDeclaration = ListStruct.toLispList(DeclarationStructImpl.JAVA_CLASS_NAME, newJavaClassName);
		final ListStruct declareBlock = ListStruct.toLispList(SpecialOperatorStructImpl.DECLARE, javaClassNameDeclaration);

		final ListStruct formsToCompile = ListStruct.toLispList(forms);
		return (ListStruct) ListStruct.toLispDottedList(SpecialOperatorStructImpl.LAMBDA, NILStruct.INSTANCE, declareBlock, formsToCompile);
	}

	private static void writeToJar(final Deque<JavaClassBuilder> javaClassBuilderDeque, final Path outputFilePath, final String inputFileName,
	                               final String inputClassName, final boolean print)
			throws IOException {

		final String tempFileName = "TEMP_" + inputFileName + "_JAR_" + System.nanoTime();
		final Path tempOutputFilePath = Files.createTempFile(tempFileName, ".jar");

		final Manifest manifest = new Manifest();
		final Attributes manifestMainAttributes = manifest.getMainAttributes();
		manifestMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
		manifestMainAttributes.put(Attributes.Name.MAIN_CLASS, inputClassName);

		try (final OutputStream outputStream = Files.newOutputStream(tempOutputFilePath, StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		     final JarOutputStream jar = new JarOutputStream(outputStream, manifest)) {

			for (final JavaClassBuilder javaClassBuilder : javaClassBuilderDeque) {
				final ClassWriter cw = javaClassBuilder.getClassWriter();

				final byte[] byteArray = cw.toByteArray();
				final ClassReader cr = new ClassReader(byteArray);

				final CheckClassAdapter cca = new CheckClassAdapter(new ClassWriter(0), false);
				cr.accept(cca, ClassReader.SKIP_DEBUG + ClassReader.SKIP_FRAMES);

				if (print) {
					final String fileName = javaClassBuilder.getFileName();
					log.info("; Compiled '{}'", fileName);
				}

				final String className = javaClassBuilder.getClassName();
				final String entryClassName = className + ".class";
				final JarEntry entry = new JarEntry(entryClassName);
				jar.putNextEntry(entry);
				jar.write(byteArray);
				jar.closeEntry();
			}
		}

		Files.move(tempOutputFilePath, outputFilePath, StandardCopyOption.REPLACE_EXISTING);
	}

	public static PathnameStruct compileFilePathname(final LispStruct inputFile, final LispStruct outputFile) {
		final LispStruct realOutputFile = ((outputFile == null) || (outputFile == NILStruct.INSTANCE))
		                                  ? null
		                                  : outputFile;

		final PathnameStruct inputFilePathname = PathnameStruct.toPathname(inputFile);
		final PathnameStruct defaultPathnameDefaults = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
		final PathnameStruct mergedInputFile = PathnameStruct.mergePathnames(inputFilePathname, defaultPathnameDefaults);

		final PathnameType outputPathnameType = new PathnameType("jar");

		final boolean isLogicalInputFile = mergedInputFile instanceof LogicalPathnameStruct;

		if ((realOutputFile == null) && isLogicalInputFile) {
			final PathnameStruct translatedMergedInputFile = PathnameStruct.translateLogicalPathname(mergedInputFile);
			return LogicalPathnameStruct.toLogicalPathname(
					translatedMergedInputFile.getPathnameHost(),
					translatedMergedInputFile.getPathnameDirectory(),
					translatedMergedInputFile.getPathnameName(),
					outputPathnameType,
					translatedMergedInputFile.getPathnameVersion()
			);
		} else if (isLogicalInputFile) {
			final PathnameStruct outputFilePathname = PathnameStruct.toPathname(realOutputFile);
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
		} else if (realOutputFile == null) {
			return PathnameStruct.toPathname(
					mergedInputFile.getPathnameHost(),
					mergedInputFile.getPathnameDevice(),
					mergedInputFile.getPathnameDirectory(),
					mergedInputFile.getPathnameName(),
					outputPathnameType,
					mergedInputFile.getPathnameVersion()
			);
		} else {
			final PathnameStruct outputFilePathname = PathnameStruct.toPathname(realOutputFile);
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
