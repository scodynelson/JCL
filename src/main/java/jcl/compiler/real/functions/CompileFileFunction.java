package jcl.compiler.real.functions;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
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

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.functions.PathnameFunction;
import jcl.printer.Printer;
import jcl.reader.functions.ReadFunction;
import jcl.streams.FileStreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.TStruct;
import org.apache.commons.lang3.StringUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CompileFileFunction {

	public static final CompileFileFunction FUNCTION = new CompileFileFunction();

	private static final Logger LOGGER = LoggerFactory.getLogger(CompileFileFunction.class);

	private CompileFileFunction() {
	}

	@Autowired
	private Printer printer;

	@Autowired
	private SemanticAnalyzer semanticAnalyzer;

	@Autowired
	private IntermediateCodeGenerator intermediateCodeGenerator;

	@Autowired
	private CompileFilePathnameFunction compileFilePathnameFunction;

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private PathnameFunction pathnameFunction;

	public Object apply(final ListStruct args) {
		return null;
	}

	public Object compileFile(final LispStruct inputFile, final LispStruct outputFile, final boolean verbose,
	                          final boolean print, final boolean writeFile) {



		final PathnameStruct inputFilePathname = pathnameFunction.pathname(inputFile);
		final Path inputFilePath = inputFilePathname.getPath();

		final boolean inputFileNotExists = Files.notExists(inputFilePath);
		if (inputFileNotExists) {
			throw new ProgramErrorException("Input file provided to COMPILE-FILE does not exist: " + inputFilePath);
		}

		final String inputFileNamestring = inputFilePath.toString();
		if (!StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lsp") && !StringUtils.endsWithIgnoreCase(inputFileNamestring, ".lisp")) {
			throw new RuntimeException("File to compile must be of type .lsp or .lisp");
		}

		final Instant startTime = Instant.now();
		if (verbose) {
			final String javaVersion = System.getProperty("java.version");
			LOGGER.info("; Java Compiler Version {}", javaVersion);

			final LocalDateTime now = LocalDateTime.now();
			LOGGER.info("; Compiling {} on {}", inputFileNamestring, now);
			LOGGER.info("");
		}

		final PathnameStruct outputFilePathname = compileFilePathnameFunction.compileFilePathname(inputFilePathname, outputFile);

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		BooleanStruct failedToCompile = NILStruct.INSTANCE;

		final LispStruct previousCompileFilePathname = CompilerVariables.COMPILE_FILE_PATHNAME.getValue();
		final LispStruct previousCompileFileTruename = CompilerVariables.COMPILE_FILE_TRUENAME.getValue();

		boolean compiledSuccessfully = false;
		try {
			final FileStreamStruct inputFileStream = new FileStreamStruct(inputFilePath);

			final List<LispStruct> forms = new ArrayList<>();

			LispStruct form;
			do {
				form = readFunction.read(inputFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

				if (form instanceof ListStruct) {
					forms.add(form);
				} else {
					final String printedForm = printer.print(form);
					final Long currentFilePosition = inputFileStream.filePosition(null);
					LOGGER.debug("Deleted a non-list form {} found at line {}.", printedForm, currentFilePosition);
					form = NullStruct.INSTANCE;
				}
			} while (form != null);

			if (print) {
				/* TODO: where do we add this information in???
; Converted FOO.
; Compiling DEFUN FOO:
; Converted BAR.
; Compiling DEFUN BAR:
; Compiling LAMBDA NIL:
; Byte Compiling Top-Level Form:
				 */
			}

			final String name = StringUtils.capitalize(inputFilePath.getFileName().toString());
			final StringStruct newJavaClassName = new StringStruct(name);
			final ListStruct javaClassNameDeclaration = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, newJavaClassName);
			final ListStruct declareBlock = ListStruct.buildProperList(SpecialOperatorStruct.DECLARE, javaClassNameDeclaration);

			final ListStruct formsToCompile = ListStruct.buildProperList(forms);
			final ListStruct fileLambdaForm = ListStruct.buildDottedList(SpecialOperatorStruct.LAMBDA, NullStruct.INSTANCE, declareBlock, formsToCompile);

			final LambdaStruct analyzedFileLambda = semanticAnalyzer.analyze(fileLambdaForm);
			final Deque<ClassDef> classDefDeque = intermediateCodeGenerator.generate(analyzedFileLambda);

			final Manifest manifest = new Manifest();
			final Attributes manifestMainAttributes = manifest.getMainAttributes();
			manifestMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");

			final ClassDef mainClassDef = classDefDeque.getFirst();
			final String mainClassDefName = mainClassDef.getName();
			manifestMainAttributes.put(Attributes.Name.MAIN_CLASS, mainClassDefName);

			final String tempFileName = "TEMP_" + name + "_JAR_" + System.nanoTime();
			final Path tempFile = Files.createTempFile(tempFileName, ".jar");

			final Path outputFilePath = outputFilePathname.getPath();
			final OutputStream outputStream = Files.newOutputStream(outputFilePath, StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
			try (final JarOutputStream jar = new JarOutputStream(outputStream, manifest)) {

				for (final ClassDef classDef : classDefDeque) {
					final ClassWriter cw = classDef.getClassWriter();

					final byte[] byteArray = cw.toByteArray();
					final ClassReader cr = new ClassReader(byteArray);

					final CheckClassAdapter cca = new CheckClassAdapter(new ClassWriter(0), false);
					cr.accept(cca, ClassReader.SKIP_DEBUG + ClassReader.SKIP_FRAMES);

					if (writeFile) {
						final String className = classDef.getName() + ".class";
						final JarEntry entry = new JarEntry(className);
						jar.putNextEntry(entry);
						jar.write(byteArray);
						jar.closeEntry();
					}
				}
			}

			if (writeFile) {
				Files.move(tempFile, outputFilePath, StandardCopyOption.REPLACE_EXISTING);
			} else {
				Files.deleteIfExists(tempFile);
			}

			compiledWithWarnings = TStruct.INSTANCE;
			failedToCompile = TStruct.INSTANCE;

			compiledSuccessfully = true;
		} catch (final FileNotFoundException e) {
			compiledSuccessfully = false;

			LOGGER.error("FileNotFoundException", e);
			throw new RuntimeException("File " + inputFilePath + " does not exist.");
		} catch (final IOException e) {
			compiledSuccessfully = false;

			LOGGER.error("IOException", e);
			throw new RuntimeException("Unable to view contents of File " + inputFilePath);
		} catch (final IllegalArgumentException e) {
			compiledSuccessfully = false;

			LOGGER.error("IllegalArgumentException", e);
			throw new RuntimeException(e);
		} finally {

			if (compiledSuccessfully && verbose) {
				final String outputFilePathnameString = "";
				LOGGER.info("; {} written", outputFilePathnameString);

				final LocalDateTime now = LocalDateTime.now();
				LOGGER.info("; Compilation finished in {}.", now);
			} else if (verbose) {
				final Instant endTime = Instant.now();
				final Duration duration = Duration.between(startTime, endTime);
				LOGGER.info("; Compilation aborted after {}.", duration);
			}

			CompilerVariables.COMPILE_FILE_TRUENAME.setValue(previousCompileFileTruename);
			CompilerVariables.COMPILE_FILE_PATHNAME.setValue(previousCompileFilePathname);
		}

		final LispStruct valuesFirst = (outputFile == null) ? NullStruct.INSTANCE : outputFilePathname;
		return new ValuesStruct(valuesFirst, compiledWithWarnings, failedToCompile);
	}

// TODO: Need: 'translate-logical-pathname' | 'make-pathname' | 'pathname' | 'open'


/*
(defun compile-file (source &key (output-file t)
							     (error-output t)
							     (external-format :default)
							     ((:verbose *compile-verbose*) *compile-verbose*)
							     ((:print *compile-print*) *compile-print*))

  (let* ((jar-file nil)
		 (output-file-pathname nil)
		 (compile-won nil)
		 (error-severity nil)
		 (source (verify-source-files source))
		 (source-info (make-file-source-info source external-format t))
		 (default (pathname (first source))))
    (unwind-protect

    ;; PROGN START
	  (progn
		(when output-file
		  (setq output-file-pathname
			    (translate-logical-pathname (if (eq output-file t)
												(compile-file-pathname (first source))
											  (compile-file-pathname (first source) :output-file output-file))))
		  (setq jar-file (open-jar-file output-file-pathname (namestring (first source)))))

		(when *compile-verbose*
		  (start-error-output source-info))
		(setq error-severity
			  (let ((*compile-object* jar-file))
			    (sub-compile-file source-info)))
		(setq compile-won t))
    ;; PROGN END

    ;; Unwind-Protect Cleanup Forms START
	  (close-source-info source-info)

	  (when jar-file
		(close-jar-file jar-file (not compile-won))
		(setq output-file-pathname (pathname (jar-file-stream jar-file)))
		(when (and compile-won *compile-verbose*)
		  (compiler-mumble "~2&; ~A written.~%" (namestring output-file-pathname))))

	  (when *compile-verbose*
	    (finish-error-output source-info compile-won))

    ;; Unwind-Protect Cleanup Forms END

	(values (if output-file
				output-file-pathname
			  nil)
		    ;; CLHS says the second return value "is false if no
		    ;; conditions of type error or warning were detected by
		    ;; the compiler".  This should include style-warnings.
	        (not (null error-severity))
	        ;; FIXM in the following we should not return t for a
	        ;; STYLE-WARNING
		    (if (member error-severity '(:warning :error))
		        t
		      nil))))

;;; START-ERROR-OUTPUT, FINISH-ERROR-OUTPUT  --  Internal
;;;
;;;    Print some junk at the beginning and end of compilation.
;;;
(defun start-error-output (source-info)
  (declare (type source-info source-info))
  (compiler-mumble "~2&; Python version ~A, VM version ~A on ~A.~%"
		            compiler-version
		            (backend-version *backend*)
		            (ext:format-universal-time nil (get-universal-time) :style :iso8601
																	    :print-weekday nil
																	    :print-timezone nil))
  (dolist (x (source-info-files source-info))
    (compiler-mumble "; Compiling: ~A ~A~%"
				     (namestring (file-info-name x))
				     (ext:format-universal-time nil (file-info-write-date x) :style :iso8601
																			 :print-weekday nil
																			 :print-timezone nil)))
  (compiler-mumble "~%")
  (undefined-value))
;;;
(defun finish-error-output (source-info won)
  (declare (type source-info source-info))
  (compiler-mumble "~&; Compilation ~:[aborted after~;finished in~] ~A.~&"
		           won
		           (elapsed-time-to-string (- (get-universal-time)
		                                      (source-info-start-time source-info))))
  (undefined-value))

*/
}
