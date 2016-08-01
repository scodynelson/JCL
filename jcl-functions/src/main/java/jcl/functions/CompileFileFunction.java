package jcl.functions;

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
import jcl.functions.pathname.PathnameFunction;
import jcl.functions.readtable.ReadFunction;
import jcl.lang.BooleanStruct;
import jcl.lang.CommonLispSymbols;
import jcl.lang.CompilerVariables;
import jcl.lang.DeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageVariables;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import jcl.lang.stream.FileStreamStruct;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFileFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COMPILE-FILE";
	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	private static final Logger LOGGER = LoggerFactory.getLogger(CompileFileFunction.class);

	private static final Pattern VALID_FILE_CLASS_NAME_PATTERN = Pattern.compile("[^a-zA-Z0-9]");

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private SemanticAnalyzer semanticAnalyzer;

	@Autowired
	private IntermediateCodeGenerator intermediateCodeGenerator;

	@Autowired
	private PathnameFunction pathnameFunction;

	@Autowired
	private CompileFilePathnameFunction compileFilePathnameFunction;

	public CompileFileFunction() {
		super("Compiles the provided input-file.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_FILE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.OUTPUT_FILE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.VERBOSE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.PRINT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct inputFile = arguments.getRequiredArgument(INPUT_FILE_ARGUMENT);
		final LispStruct outputFile = arguments.getKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD);
		final boolean verbose;
		if (arguments.hasKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD)) {
			verbose = arguments.getKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD, BooleanStruct.class).booleanValue();
		} else {
			final BooleanStruct currentCompileVerbose = CompilerVariables.COMPILE_VERBOSE.getVariableValue();
			verbose = currentCompileVerbose.booleanValue();
		}
		final boolean print;
		if (arguments.hasKeyArgument(CommonLispSymbols.PRINT_KEYWORD)) {
			print = arguments.getKeyArgument(CommonLispSymbols.PRINT_KEYWORD, BooleanStruct.class).booleanValue();
		} else {
			final BooleanStruct currentCompilePrint = CompilerVariables.COMPILE_PRINT.getVariableValue();
			print = currentCompilePrint.booleanValue();
		}
		final LispStruct externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);
		return compileFile(inputFile, outputFile, verbose, print);
	}

	public LispStruct compileFile(final LispStruct inputFile, final LispStruct outputFile, final boolean verbose, final boolean print) {
		// NOTE: 'outputFile' will be null if it is not supplied.

		final PathnameStruct inputFilePathname = pathnameFunction.pathname(inputFile);
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
			LOGGER.info("; Java Compiler Version {}", javaVersion);

			final LocalDateTime now = LocalDateTime.now();
			LOGGER.info("; Compiling '{}' on {}", inputFileNamestring, now);
			LOGGER.info("");
		}

		final PathnameStruct outputFilePathname = compileFilePathnameFunction.compileFilePathname(inputFilePathname, outputFile);
		final File outputFilePathnameFile = new File(outputFilePathname.getNamestring());
		final Path outputFilePath = outputFilePathnameFile.toPath();

		final LispStruct previousCompileFilePathname = CompilerVariables.COMPILE_FILE_PATHNAME.getValue();
		final LispStruct previousCompileFileTruename = CompilerVariables.COMPILE_FILE_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(outputFilePathname);
		final Path outputFileAbsolutePath = outputFilePath.toAbsolutePath();
		final PathnameStruct outputFileTruename = LispStructFactory.toPathname(outputFileAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(outputFileTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		boolean compiledSuccessfully = false;
		try {
			final FileStreamStruct inputFileStream = LispStructFactory.toFileStream(inputFilePath);
			final List<LispStruct> forms = new ArrayList<>();

			LispStruct form;
			do {
				form = readFunction.read(inputFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

				if (form instanceof ListStruct) {
					forms.add(form);
				} else if (form != null) {
					final Long currentFilePosition = inputFileStream.filePosition(null);
					// TODO: can we rework this to tell what line we're on???
					LOGGER.info("; Deleted a non-list form '{}' found at position {}.", form, currentFilePosition);
					form = NILStruct.INSTANCE;

					compiledWithWarnings = TStruct.INSTANCE;
				}
			} while (form != null);

			if (print && compiledWithWarnings.booleanValue()) {
				// If we printed warnings, make sure to print a newline afterwards.
				LOGGER.info("");
			}

			final String inputFileName = inputFilePath.getFileName().toString();
			String inputClassName = FilenameUtils.getBaseName(inputFileName);
			inputClassName = StringUtils.capitalize(inputClassName);
			inputClassName = VALID_FILE_CLASS_NAME_PATTERN.matcher(inputClassName).replaceAll("_");
			final ListStruct fileLambda = buildFileLambda(forms, inputClassName);

			final LambdaStruct analyzedFileLambda = semanticAnalyzer.analyze(fileLambda);
			final Deque<JavaClassBuilder> javaClassBuilderDeque = intermediateCodeGenerator.generate(analyzedFileLambda);

			writeToJar(javaClassBuilderDeque, outputFilePath, inputFileName, inputClassName, print);
			compiledSuccessfully = true;

			return ValuesStruct.valueOf(outputFileTruename, compiledWithWarnings, NILStruct.INSTANCE);
		} catch (final IOException e) {
			compiledSuccessfully = false;

			LOGGER.error("Error in COMPILE-FILE for file: '{}'", inputFilePath, e);

			return ValuesStruct.valueOf(NILStruct.INSTANCE, compiledWithWarnings, TStruct.INSTANCE);
		} finally {
			if (compiledSuccessfully && verbose) {
				LOGGER.info("\n; '{}' written", outputFilePath);

				final LocalDateTime now = LocalDateTime.now();
				LOGGER.info("; Compilation finished in {}.", now);
			} else if (verbose) {
				final Instant endTime = Instant.now();
				final Duration duration = Duration.between(startTime, endTime);
				LOGGER.info("; Compilation aborted after {}.", duration);
			}
			LOGGER.info("");

			CompilerVariables.COMPILE_FILE_TRUENAME.setValue(previousCompileFileTruename);
			CompilerVariables.COMPILE_FILE_PATHNAME.setValue(previousCompileFilePathname);

			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READTABLE.setValue(previousReadtable);
		}
	}

	private static ListStruct buildFileLambda(final List<LispStruct> forms, final String inputClassName) {
		final StringStruct newJavaClassName = LispStructFactory.toString("jcl." + inputClassName);
		final ListStruct javaClassNameDeclaration = LispStructFactory.toProperList(DeclarationStruct.JAVA_CLASS_NAME, newJavaClassName);
		final ListStruct declareBlock = LispStructFactory.toProperList(SpecialOperatorStruct.DECLARE, javaClassNameDeclaration);

		final ListStruct formsToCompile = LispStructFactory.toProperList(forms);
		return LispStructFactory.toDottedList(SpecialOperatorStruct.LAMBDA, NILStruct.INSTANCE, declareBlock, formsToCompile);
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
					LOGGER.info("; Compiled '{}'", fileName);
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
}
