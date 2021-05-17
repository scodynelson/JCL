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
import java.util.Deque;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Pattern;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.sa.FormAnalyzer;
import jcl.lang.BooleanStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.util.CheckClassAdapter;

@Log4j2
@UtilityClass
public final class InternalCompile {

	static final Attributes.Name LISP_MODULE_NAME = new Attributes.Name("Lisp-Module-Name");

	private static final Pattern VALID_FILE_CLASS_NAME_PATTERN = Pattern.compile("[^a-zA-Z0-9]");

	private static final String MAIN_METHOD_NAME = "main";
	private static final String MAIN_METHOD_DESC = "([Ljava/lang/String;)V";
	private static final String INIT_METHOD_NAME = "init";
	private static final String INIT_METHOD_DESC = "()V";

	public static CompileResult compile(final LispStruct name, final LispStruct uncompiledDefinition) {

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
				nameSymbol.setSymbolFunction(function);
			} else if (!NILStruct.INSTANCE.eq(name)) {
				throw new ErrorException("The value " + name + " is not an acceptable function name.");
			}

			if (compiledDefinition == null) {
				return new CompileResult(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
			} else {
				return new CompileResult(
						function,
						compiledDefinition.getWarningsP(),
						compiledDefinition.getFailureP()
				);
			}
		}

		if (!(name instanceof SymbolStruct)) {
			throw new ErrorException("The value " + name + " is not an acceptable function name.");
		}
		final SymbolStruct nameSymbol = (SymbolStruct) name;

		final MacroFunctionExpanderInter macroFunction = Environment.NULL.getMacroFunctionExpander(nameSymbol);
		if (macroFunction != null) {
			return new CompileResult(macroFunction, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		if (Environment.NULL.hasFunction(nameSymbol)) {
			final FunctionStruct function = nameSymbol.symbolFunction();
			return new CompileResult(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		throw new ErrorException("No definition found for " + nameSymbol);
	}

	public static CompileFileResult compileFile(final LispStruct inputFile, final LispStruct outputFile,
	                                            final BooleanStruct verboseVal, final BooleanStruct printVal,
	                                            final LispStruct externalFormat) {
		// NOTE: 'outputFile' will be null if it is not supplied.
		final boolean verbose = verboseVal.toJavaPBoolean();
		final boolean print = printVal.toJavaPBoolean();

		final PathnameStruct inputFilePathname = PathnameStruct.fromDesignator(inputFile);
		final File inputFilePathnameFile = new File(inputFilePathname.namestring());
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
			log.info("");
			final String javaVersion = SystemUtils.JAVA_VERSION;
			log.info("; Java Compiler Version {}", javaVersion);

			final LocalDateTime now = LocalDateTime.now();
			log.info("; Compiling '{}' on {}", inputFileNamestring, now);
			log.info("");
		}

		final PathnameStruct outputFilePathname = compileFilePathname(inputFilePathname, outputFile);
		final File outputFilePathnameFile = new File(outputFilePathname.namestring());
		final Path outputFilePath = outputFilePathnameFile.toPath();

		final LispStruct previousCompileFilePathname = CommonLispSymbols.COMPILE_FILE_PATHNAME_VAR.symbolValue();
		final LispStruct previousCompileFileTruename = CommonLispSymbols.COMPILE_FILE_TRUENAME_VAR.symbolValue();

		CommonLispSymbols.COMPILE_FILE_PATHNAME_VAR.setSymbolValue(outputFilePathname);
		final PathnameStruct outputFileTruename = PathnameStruct.toPathname(outputFilePathnameFile.toURI().toString());
		CommonLispSymbols.COMPILE_FILE_TRUENAME_VAR.setSymbolValue(outputFileTruename);

		final ReadtableStruct previousReadtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();
		final PackageStruct previousPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		boolean compiledSuccessfully = false;
		try {
			final FileStreamStruct inputFileStream = FileStreamStruct.toFileStream(
					inputFilePathname, CommonLispSymbols.INPUT_KEYWORD, CommonLispSymbols.CHARACTER,
					CommonLispSymbols.NIL, CommonLispSymbols.NIL, CommonLispSymbols.DEFAULT_KEYWORD
			);

			final String inputFileLispName = StringStruct.fromDesignator(inputFilePathname.pathnameName()).toJavaString();

			final String inputFileName = inputFilePath.getFileName().toString();
			String inputClassName = FilenameUtils.getBaseName(inputFileName);
			inputClassName = StringUtils.capitalize(inputClassName);
			inputClassName = VALID_FILE_CLASS_NAME_PATTERN.matcher(inputClassName).replaceAll("_");
			inputClassName = "jcl." + inputClassName;

			//
			// CODE GENERATION FIRST - START
			//
			final GeneratorState generatorState = new GeneratorState();

			final String className = inputClassName.replace('.', '/');
			final String fileName = CodeGenerators.getFileNameFromClassName(className);

			final JavaClassBuilder currentClass = new JavaClassBuilder(className, fileName);
			final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

			classBuilderDeque.addFirst(currentClass);
			generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

			final ClassWriter cw = currentClass.getClassWriter();

			cw.visit(Opcodes.V15, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
			                  className,
			                  null,
			                  GenerationConstants.JAVA_OBJECT_NAME,
			                  null);

			cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

			{
				final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
				                                        GenerationConstants.INIT_METHOD_NAME,
				                                        GenerationConstants.INIT_METHOD_DESC,
				                                        null,
				                                        null);
				mv.visitCode();
				mv.visitVarInsn(Opcodes.ALOAD, 0);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				                   GenerationConstants.JAVA_OBJECT_NAME,
				                   GenerationConstants.INIT_METHOD_NAME,
				                   GenerationConstants.INIT_METHOD_DESC,
				                   false);
				mv.visitInsn(Opcodes.RETURN);
				mv.visitMaxs(1, 1);
				mv.visitEnd();
			}
			{
				final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
				                                        MAIN_METHOD_NAME,
				                                        MAIN_METHOD_DESC,
				                                        null,
				                                        null);
				mv.visitCode();
				mv.visitTypeInsn(Opcodes.NEW, className);
				mv.visitInsn(Opcodes.DUP);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				                   className,
				                   GenerationConstants.INIT_METHOD_NAME,
				                   GenerationConstants.INIT_METHOD_DESC,
						 false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   className,
				                   INIT_METHOD_NAME,
				                   INIT_METHOD_DESC,
				                   false);
				mv.visitInsn(Opcodes.RETURN);
				mv.visitMaxs(-1, -1);
				mv.visitEnd();
			}

			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
			                                        INIT_METHOD_NAME,
			                                        INIT_METHOD_DESC,
			                                        null,
			                                        null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
			methodBuilderDeque.addFirst(methodBuilder);

			mv.visitCode();
			@SuppressWarnings({"unused", "SuppressionAnnotation"})
			final int thisStore = methodBuilder.getNextAvailableStore();
			@SuppressWarnings({"unused", "SuppressionAnnotation"})
			final int environmentStore = methodBuilder.getNextAvailableStore();
			mv.visitFieldInsn(Opcodes.GETSTATIC,
			                  GenerationConstants.ENVIRONMENT_NAME,
			                  GenerationConstants.ENVIRONMENT_NULL_NAME,
			                  GenerationConstants.ENVIRONMENT_DESC);
			mv.visitVarInsn(Opcodes.ASTORE, environmentStore);

			final Environment globalEnvironment = new Environment(Environment.NULL);

			//
			// CODE GENERATION FIRST - END
			//

			LispStruct form;
			do {
				form = InternalRead.read(inputFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

				if (form instanceof ListStruct) {
					final LispStruct analyzedForm = FormAnalyzer.analyze(form, globalEnvironment);
					if (analyzedForm != NILStruct.INSTANCE) {
						IntermediateCodeGenerator.generate(analyzedForm, generatorState);
						mv.visitInsn(Opcodes.POP);

						final Set<SymbolStruct> undefinedFunctions = globalEnvironment.getUndefinedFunctions();

						if (!undefinedFunctions.isEmpty()) {
							undefinedFunctions.clear();
						}
					}
				} else if (form != null) {
					final LispStruct filePosition = inputFileStream.filePosition();
					final IntegerStruct lineNumber = inputFileStream.lineNumber();
					log.info("; Deleted a non-list form '{}' found at position {} on line {}.", form, filePosition, lineNumber);
					form = NILStruct.INSTANCE;

					compiledWithWarnings = TStruct.INSTANCE;
				}
			} while (form != null);

			//
			// CODE GENERATION LAST - START
			//

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderDeque.removeFirst();

			cw.visitEnd();

			classBuilderDeque.removeFirst();
			//
			// CODE GENERATION LAST - END
			//

			if (compiledWithWarnings.toJavaPBoolean()) {
				// If we printed warnings, make sure to print a newline afterwards.
				log.info("");
			}

			if (print) {
				// Print newline before each compiled class is printed
				log.info("");
			}

			final Deque<JavaClassBuilder> javaClassBuilderDeque = generatorState.getFinalClassBuilderDeque();

			writeToJar(javaClassBuilderDeque, outputFilePath, inputFileLispName, inputFileName, inputClassName, print);
			compiledSuccessfully = true;

			return new CompileFileResult(outputFileTruename, compiledWithWarnings, NILStruct.INSTANCE);
		} catch (final IOException e) {
			compiledSuccessfully = false;

			log.error("Error in COMPILE-FILE for file: '{}'", inputFilePath, e);

			return new CompileFileResult(NILStruct.INSTANCE, compiledWithWarnings, TStruct.INSTANCE);
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

			CommonLispSymbols.COMPILE_FILE_TRUENAME_VAR.setSymbolValue(previousCompileFileTruename);
			CommonLispSymbols.COMPILE_FILE_PATHNAME_VAR.setSymbolValue(previousCompileFilePathname);

			CommonLispSymbols.PACKAGE_VAR.setSymbolValue(previousPackage);
			CommonLispSymbols.READTABLE_VAR.setSymbolValue(previousReadtable);
		}
	}

	private static void writeToJar(final Deque<JavaClassBuilder> javaClassBuilderDeque, final Path outputFilePath,
	                               final String inputFileLispName, final String inputFileName,
	                               final String inputClassName, final boolean print)
			throws IOException {

		final String tempFileName = "TEMP_" + inputFileName + "_JAR_" + System.nanoTime();
		final Path tempOutputFilePath = Files.createTempFile(tempFileName, ".jar");

		final Manifest manifest = new Manifest();
		final Attributes manifestMainAttributes = manifest.getMainAttributes();
		manifestMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
		manifestMainAttributes.put(Attributes.Name.MAIN_CLASS, inputClassName);
		manifestMainAttributes.put(LISP_MODULE_NAME, inputFileLispName);

		try (final OutputStream outputStream = Files.newOutputStream(tempOutputFilePath, StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		     final JarOutputStream jar = new JarOutputStream(outputStream, manifest)) {

			for (final JavaClassBuilder javaClassBuilder : javaClassBuilderDeque) {
				final ClassWriter cw = javaClassBuilder.getClassWriter();

				final byte[] byteArray = cw.toByteArray();
				CompileForm.outputCompiledClassFile(javaClassBuilder, byteArray);

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
//		final PathnameStruct defaults = CommonLispSymbols.DEFAULT_PATHNAME_DEFAULTS_VAR.getVariableValue();

		final PathnameStruct inputFilePathname = PathnameStruct.fromDesignator(inputFile);

		final PathnameStruct outputFilePathname;
		if (NILStruct.INSTANCE.eq(outputFile)) {
//			final PathnameStruct outputDefaults = PathnameStructs.mergePathnames(inputFilePathname, defaults);
			final PathnameStruct outputDefaults = inputFilePathname;

			outputFilePathname = PathnameStruct.toPathname(
					outputDefaults.pathnameHost(),
					outputDefaults.pathnameDevice(),
					outputDefaults.pathnameDirectory(),
					outputDefaults.pathnameName(),
					StringStruct.toLispString("jar"),
					outputDefaults.pathnameVersion()
			);
		} else {
			outputFilePathname = PathnameStruct.fromDesignator(outputFile);
		}

//		final PathnameStruct outputFilePathnameDefaults = PathnameStructs.mergePathnames(inputFilePathname, defaults);
//		return PathnameStructs.mergePathnames(outputFilePathname, outputFilePathnameDefaults, NILStruct.INSTANCE);
		return outputFilePathname;
	}
}
