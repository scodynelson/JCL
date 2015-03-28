package jcl.compiler.real.functions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Scanner;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Pattern;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.functions.OpenFunction;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.functions.TranslateLogicPathnameFunction;
import jcl.printer.Printer;
import jcl.reader.functions.ReadFunction;
import jcl.streams.FileStreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
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
	private CompileFilePathname compileFilePathname;

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private TranslateLogicPathnameFunction translateLogicPathnameFunction;

	public Object apply(final ListStruct args) {
		return null;
	}

	public Object compileFile(final PathnameStruct inputFile, final PathnameStruct outputFile, final BooleanStruct verbose,
	                          final BooleanStruct print, final LispStruct externalFormat, final boolean writeFile) {


		// TODO: next verify the file with the pathname actually exists
		// if not, throw error: "Can't compile with no source files." or something to that effect

		final PathnameType pathnameType = inputFile.getPathnameType();
		final String type = pathnameType.getType();
		final boolean correctInputType = type.endsWith(".lsp") || type.endsWith(".lisp");

		if (!correctInputType) {
			throw new RuntimeException("file to compile must be of type .lsp or .lisp");
		}

		PathnameStruct outputFilePathname = null;

		Object jarFile = null;

		final PathnameStruct outputCompileFilePathname;
		if (outputFile == null) {
			outputCompileFilePathname = compileFilePathname.compileFilePathname(inputFile, null);
		} else {
			outputCompileFilePathname = compileFilePathname.compileFilePathname(inputFile, outputFile);
		}
		outputFilePathname = translateLogicPathnameFunction.translateLogicalPathname(outputFilePathname);

		// TODO: (open-jar-file output-file-pathname (namestring (first source)))

		final boolean compileVerbose = CompilerVariables.COMPILE_VERBOSE.getValue().booleanValue();

		final Instant startTime = Instant.now();
		if (compileVerbose) {
			final String javaVersion = System.getProperty("java.version");
			LOGGER.info("Java Compiler Version {}", javaVersion);

			final String fileName = inputFile.toString();
			final LocalDateTime now = LocalDateTime.now();
			LOGGER.info("Compiling {} on {}", fileName, now);
			LOGGER.info("");
		}

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		BooleanStruct failedToCompile = NILStruct.INSTANCE;

		boolean compiledSuccessfully = false;
		try {
			compiledSuccessfully = subCompileFile(outputFile, writeFile);

			compiledWithWarnings = TStruct.INSTANCE;
			failedToCompile = TStruct.INSTANCE;
		} catch (final FileNotFoundException e) {
			LOGGER.error("FileNotFoundException", e);
			throw new RuntimeException("File " + inputFile + " does not exist.");
		} catch (final IOException e) {
			LOGGER.error("IOException", e);
			throw new RuntimeException("Unable to view contents of File " + inputFile);
		} catch (final IllegalArgumentException e) {
			LOGGER.error("IllegalArgumentException", e);
			throw new RuntimeException(e);
		} finally {

			if (compiledSuccessfully && compileVerbose) {
				final String outputFilePathnameString = "";
				LOGGER.info("{} written", outputFilePathnameString);

				final LocalDateTime now = LocalDateTime.now();
				LOGGER.info("Compilation finished in {}.", now);
			} else if (compileVerbose) {
				final Instant endTime = Instant.now();
				final Duration duration = Duration.between(startTime, endTime);
				LOGGER.info("Compilation aborted after {}.", duration);
			}
		}

		final LispStruct valuesFirst = (outputFile == null) ? NullStruct.INSTANCE : outputFilePathname;
		return new ValuesStruct(valuesFirst, compiledWithWarnings, failedToCompile);
	}

	private boolean subCompileFile(final LispStruct outputFile, final boolean writeFile) throws IOException {
		// get the name and javafy it
		String name = ""; // TODOfile.getName();
		String path = ""; // TODOfile.toString();
		final String nameRegex = "(\\p{Punct}|\\p{Space})+";
		final Pattern namePattern = Pattern.compile(nameRegex);
		final Scanner scanner = new Scanner(name).useDelimiter(namePattern);
		final StringBuilder sb = new StringBuilder();
		while (scanner.hasNext()) {
			final String str = scanner.next();
			sb.append("").append(Character.toUpperCase(str.charAt(0))).append(str.substring(1));
		}
		final SymbolStruct<?> newJavaClassName = new SymbolStruct<>(sb.toString());

		final FileStreamStruct stream = (FileStreamStruct) OpenFunction.FUNCTION.funcall(ListStruct.buildProperList(new StringStruct(path), null, null, KeywordOld.NewVersion));
		final List<LispStruct> formsToCompile = getFormsToCompile(stream);

		// Create the wrap-around lambda expression that encloses all of the forms in the file.
		// We have to give it a specific name so it can be loaded by name
		ListStruct formList = ListStruct.buildProperList(formsToCompile);

		final ListStruct nameDeclSpec = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, newJavaClassName);
		final ListStruct nameDecl = ListStruct.buildProperList(SpecialOperatorStruct.DECLARE, nameDeclSpec);
		formList = new ConsStruct(nameDecl, formList);
		formList = new ConsStruct(NullStruct.INSTANCE, formList);
		formList = new ConsStruct(SpecialOperatorStruct.LAMBDA, formList);


		final LambdaStruct analyzedObj = semanticAnalyzer.analyze(formList);
		final Deque<ClassDef> classDefDeque = intermediateCodeGenerator.generate(analyzedObj);
		if (classDefDeque.isEmpty()) {
			// TODO: handle no classes!!!
			return true;
		}

		File tmpFile = null;
		JarOutputStream jar = null;
		if (writeFile) {
			final Manifest manifest = new Manifest();
			manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
			manifest.getMainAttributes().put(Attributes.Name.MAIN_CLASS, classDefDeque.getFirst());

			tmpFile = File.createTempFile("TMP_JAR_" + System.currentTimeMillis(), ".jar");
			final FileOutputStream fileStream = new FileOutputStream(tmpFile, false);
			jar = new JarOutputStream(fileStream, manifest);
		}

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		BooleanStruct failedToCompile = NILStruct.INSTANCE;

		FunctionStruct function = null;
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

		if (writeFile) {
			final File outputFileFile = new File(String.valueOf(outputFile));
			if (outputFileFile.exists()) {
				LOGGER.info("Closing and deleting {}", outputFileFile);
				//				new JarFile(outputFileFile).close();
				outputFileFile.delete();
			}

			final boolean renameResult = tmpFile.renameTo(outputFileFile);
			if (!renameResult) {

				LOGGER.warn("Warning: Unable to rename temp file: {}. JAR file not created.", tmpFile);
				LOGGER.warn("\t Attempting to copy then delete temp file instead...");

				// copying file to destination
				final FileInputStream srcFile = new FileInputStream(tmpFile);
				final FileOutputStream dstFile = new FileOutputStream(outputFileFile);
				final byte[] buffer = new byte[1024];

				for (int x = srcFile.read(buffer); x != -1; x = srcFile.read(buffer)) {
					dstFile.write(buffer, 0, x);
				}

				srcFile.close();
				dstFile.close();

				// deleting temp jar file
				if (!tmpFile.delete()) {
					LOGGER.warn("WARNING: Failed to delete temp jar file.");
				}
			}
		}
		return false;
	}

	private List<LispStruct> getFormsToCompile(final FileStreamStruct file) {
		final List<LispStruct> forms = new ArrayList<>();

		LispStruct form;
		do {
			form = readFunction.read(file, NILStruct.INSTANCE, null, NILStruct.INSTANCE);

			if (form instanceof ListStruct) {
				forms.add(form);
			} else {
				final String printedForm = printer.print(form);
				final Long currentFilePosition = file.filePosition(null);
				LOGGER.debug("Deleted a non-list form {} found at line {}.", printedForm, currentFilePosition);
				form = NullStruct.INSTANCE;
			}
		} while (form != null);

		return forms;
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
