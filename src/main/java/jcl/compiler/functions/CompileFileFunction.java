package jcl.compiler.functions;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Pattern;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.CompilerVariables;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.sa.SemanticAnalyzer;
import jcl.compiler.struct.ValuesStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.FileErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.functions.PathnameFunction;
import jcl.printer.Printer;
import jcl.reader.functions.ReadFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.FileStreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
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
public final class CompileFileFunction extends FunctionStruct {

	public static final SymbolStruct<?> COMPILE_FILE = GlobalPackageStruct.COMMON_LISP.intern("COMPILE-FILE").getSymbol();

	private static final long serialVersionUID = -3067892826539388846L;

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

	@Autowired
	private Printer printer;

	private CompileFileFunction() {
		super("Compiles the provided input-file.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		COMPILE_FILE.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(COMPILE_FILE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> inputFileArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INPUT-FILE").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(inputFileArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final List<KeyParameter> keyBindings = new ArrayList<>(4);

		final SymbolStruct<?> outputFileArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OUTPUT-FILE").getSymbol();

		final SymbolStruct<?> outputFileSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("OUTPUT-FILE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter outputFileSuppliedPBinding = new SuppliedPParameter(outputFileSuppliedP);

		final KeyParameter outputFileKeyBinding = new KeyParameter(outputFileArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.OUTPUT_FILE_KEYWORD, outputFileSuppliedPBinding);
		keyBindings.add(outputFileKeyBinding);

		final SymbolStruct<?> verboseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("VERBOSE").getSymbol();

		final SymbolStruct<?> verboseSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("VERBOSE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter verboseSuppliedPBinding = new SuppliedPParameter(verboseSuppliedP);

		final KeyParameter verboseKeyBinding = new KeyParameter(verboseArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.VERBOSE_KEYWORD, verboseSuppliedPBinding);
		keyBindings.add(verboseKeyBinding);

		final SymbolStruct<?> printArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PRINT").getSymbol();

		final SymbolStruct<?> printSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("PRINT-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter printSuppliedPBinding = new SuppliedPParameter(printSuppliedP);

		final KeyParameter printKeyBinding = new KeyParameter(printArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.PRINT_KEYWORD, printSuppliedPBinding);
		keyBindings.add(printKeyBinding);

		final SymbolStruct<?> externalFormatArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("EXTERNAL-FORMAT").getSymbol();

		final SymbolStruct<?> externalFormatSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("EXTERNAL-FORMAT-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter externalFormatSuppliedPBinding = new SuppliedPParameter(externalFormatSuppliedP);

		final KeyParameter externalFormatKeyBinding = new KeyParameter(externalFormatArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, externalFormatSuppliedPBinding);
		keyBindings.add(externalFormatKeyBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .keyBindings(keyBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct inputFile = lispStructs[0];

		final BooleanStruct currentCompileVerbose = CompilerVariables.COMPILE_VERBOSE.getValue();
		final BooleanStruct currentCompilePrint = CompilerVariables.COMPILE_PRINT.getValue();

		LispStruct outputFile = null;
		boolean verbose = currentCompileVerbose.booleanValue();
		boolean print = currentCompilePrint.booleanValue();

		final int length = lispStructs.length;
		if (length >= 3) {
			// 1 keyword
			final LispStruct firstKeyword = lispStructs[1];
			if (CommonLispSymbols.OUTPUT_FILE_KEYWORD.equals(firstKeyword)) {
				outputFile = lispStructs[2];
			} else if (CommonLispSymbols.VERBOSE_KEYWORD.equals(firstKeyword)) {
				verbose = ((BooleanStruct) lispStructs[2]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(firstKeyword)) {
				print = ((BooleanStruct) lispStructs[2]).booleanValue();
			}
		}
		if (length >= 5) {
			// 2 keywords
			final LispStruct secondKeyword = lispStructs[3];
			if (CommonLispSymbols.OUTPUT_FILE_KEYWORD.equals(secondKeyword)) {
				outputFile = lispStructs[4];
			} else if (CommonLispSymbols.VERBOSE_KEYWORD.equals(secondKeyword)) {
				verbose = ((BooleanStruct) lispStructs[4]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(secondKeyword)) {
				print = ((BooleanStruct) lispStructs[4]).booleanValue();
			}
		}
		if (length >= 7) {
			// 3 keywords
			final LispStruct thirdKeyword = lispStructs[5];
			if (CommonLispSymbols.OUTPUT_FILE_KEYWORD.equals(thirdKeyword)) {
				outputFile = lispStructs[6];
			} else if (CommonLispSymbols.VERBOSE_KEYWORD.equals(thirdKeyword)) {
				verbose = ((BooleanStruct) lispStructs[6]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(thirdKeyword)) {
				print = ((BooleanStruct) lispStructs[6]).booleanValue();
			}
		}
		if (length >= 9) {
			// 4 keywords
			final LispStruct fourthKeyword = lispStructs[7];
			if (CommonLispSymbols.OUTPUT_FILE_KEYWORD.equals(fourthKeyword)) {
				outputFile = lispStructs[8];
			} else if (CommonLispSymbols.VERBOSE_KEYWORD.equals(fourthKeyword)) {
				verbose = ((BooleanStruct) lispStructs[8]).booleanValue();
			} else if (CommonLispSymbols.PRINT_KEYWORD.equals(fourthKeyword)) {
				print = ((BooleanStruct) lispStructs[8]).booleanValue();
			}
		}
		return compileFile(inputFile, outputFile, verbose, print);
	}

	public LispStruct compileFile(final LispStruct inputFile, final LispStruct outputFile, final boolean verbose, final boolean print) {
		// NOTE: 'outputFile' will be null if it is not supplied.

		final PathnameStruct inputFilePathname = pathnameFunction.pathname(inputFile);
		final URI inputFilePathnameURI = inputFilePathname.getUri();
		final File inputFilePathnameFile = new File(inputFilePathnameURI.toString());
		final Path inputFilePath = inputFilePathnameFile.toPath();

		final boolean inputFileNotExists = Files.notExists(inputFilePath);
		if (inputFileNotExists) {
			throw new FileErrorException("Input file provided to COMPILE-FILE does not exist: " + inputFilePath);
		}

		final String inputFileNamestring = inputFilePath.toString();
		if (!StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lsp") && !StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lisp")) {
			throw new FileErrorException("Input file provided to COMPILE-FILE must have an extension of '.lsp' or '.lisp'");
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
		final URI outputFilePathnameURI = outputFilePathname.getUri();
		final File outputFilePathnameFile = new File(outputFilePathnameURI.toString());
		final Path outputFilePath = outputFilePathnameFile.toPath();

		final LispStruct previousCompileFilePathname = CompilerVariables.COMPILE_FILE_PATHNAME.getValue();
		final LispStruct previousCompileFileTruename = CompilerVariables.COMPILE_FILE_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(outputFilePathname);
		final Path outputFileAbsolutePath = outputFilePath.toAbsolutePath();
		final PathnameStruct outputFileTruename = new PathnameStruct(outputFileAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(outputFileTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getValue();

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		boolean compiledSuccessfully = false;
		try {
			final FileStreamStruct inputFileStream = new FileStreamStruct(inputFilePath);
			final List<LispStruct> forms = new ArrayList<>();

			LispStruct form;
			do {
				form = readFunction.read(inputFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

				if (form instanceof ListStruct) {
					forms.add(form);
				} else if (form != null) {
					final String printedForm = printer.print(form);
					final Long currentFilePosition = inputFileStream.filePosition(null);
					// TODO: can we rework this to tell what line we're on???
					LOGGER.info("; Deleted a non-list form '{}' found at position {}.", printedForm, currentFilePosition);
					form = NullStruct.INSTANCE;

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

			return new ValuesStruct(outputFileTruename, compiledWithWarnings, NILStruct.INSTANCE);
		} catch (final IOException e) {
			compiledSuccessfully = false;

			LOGGER.error("Error in COMPILE-FILE for file: '{}'", inputFilePath, e);

			return new ValuesStruct(NullStruct.INSTANCE, compiledWithWarnings, TStruct.INSTANCE);
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
		final StringStruct newJavaClassName = new StringStruct(inputClassName);
		final ListStruct javaClassNameDeclaration = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, newJavaClassName);
		final ListStruct declareBlock = ListStruct.buildProperList(SpecialOperatorStruct.DECLARE, javaClassNameDeclaration);

		final ListStruct formsToCompile = ListStruct.buildProperList(forms);
		return ListStruct.buildDottedList(SpecialOperatorStruct.LAMBDA, NullStruct.INSTANCE, declareBlock, formsToCompile);
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
