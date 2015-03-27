package jcl.compiler.real.functions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.math.BigInteger;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Pattern;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.functions.OpenFunction;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.pathnames.PathnameStruct;
import jcl.reader.Reader;
import jcl.streams.FileStreamStruct;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class CompileFileFunction {

	public static final CompileFileFunction FUNCTION = new CompileFileFunction();

	private static final Logger LOGGER = LoggerFactory.getLogger(CompileFileFunction.class);

	private CompileFileFunction() {
	}

	@Autowired
	private ApplicationContext context;

	@Autowired
	private SemanticAnalyzer semanticAnalyzer;

	@Autowired
	private IntermediateCodeGenerator intermediateCodeGenerator;

	@Autowired
	private MacroExpandFunction macroExpand;

	@Autowired
	private EvalFunction evalFunction;

	public Object apply(final ListStruct args) {
		final Object input = args.getFirst();
		final Object output = args.getRest().getFirst();
		final Object verbose = args.getRest().getRest().getFirst();
		final long duration = System.currentTimeMillis();


		PathnameStruct inputFile = null;

		// TODO: next verify the file with the pathname actually exists
		// if not, throw error: "Can't compile with no source files." or something to that effect


		final String inputAsString = input.toString();
		final boolean correctInputType = inputAsString.endsWith(".lsp") || inputAsString.endsWith(".lisp");

		if (!correctInputType) {
			throw new RuntimeException("file to compile must be of type .lsp or .lisp");
		}
		final PackageStruct oldPackageVarValue = PackageVariables.PACKAGE.getValue();
		PackageVariables.PACKAGE.setValue(PackageVariables.PACKAGE.getValue());

		try {
			// bind *package* to itself
			final String outputFile;
			final File file = new File(input.toString());

			if (output == NullStruct.INSTANCE) {
				String temp = file.getAbsolutePath();
				if (inputAsString.endsWith(".lsp")) {
					temp = temp.substring(0, temp.length() - 4);
				} else {
					temp = temp.substring(0, temp.length() - 5);
				}
				outputFile = temp + ".jar";
			} else {
				//Need to associate this value with the correct keyword
				outputFile = output.toString();
			}

			// get the name and javafy it
			String name = file.getName();
			final String nameRegex = "(\\p{Punct}|\\p{Space})+";
			final Pattern namePattern = Pattern.compile(nameRegex);
			final Scanner scanner = new Scanner(name).useDelimiter(namePattern);
			final StringBuilder sb = new StringBuilder();
			while (scanner.hasNext()) {
				final String str = scanner.next();
				sb.append("").append(Character.toUpperCase(str.charAt(0))).append(str.substring(1));
			}
			final SymbolStruct<?> newJavaClassName = new SymbolStruct<>(sb.toString());

			final FileStreamStruct stream = (FileStreamStruct) OpenFunction.FUNCTION.funcall(ListStruct.buildProperList(new StringStruct(file.toString()), null, null, KeywordOld.NewVersion));
			final List<List<LispStruct>> formsInfo = readForms(stream);
			final List<LispStruct> formsToCompile = formsInfo.get(0);

			// Create the wrap-around lambda expression that encloses all of the forms in the file.
			// We have to give it a specific name so it can be loaded by name
			ListStruct formList = ListStruct.buildProperList(formsToCompile);
			final ListStruct nameDeclSpec = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, newJavaClassName);
			final ListStruct nameDecl = ListStruct.buildProperList(SpecialOperatorStruct.DECLARE, nameDeclSpec);
			formList = new ConsStruct(nameDecl, formList);
			formList = new ConsStruct(NullStruct.INSTANCE, formList);
			formList = new ConsStruct(SpecialOperatorStruct.LAMBDA, formList);

			// set up the timers...

			long baseTime = System.currentTimeMillis();
			final LambdaStruct lambdaForm = semanticAnalyzer.analyze(formList);
			final long saTime = System.currentTimeMillis() - baseTime;

			baseTime = System.currentTimeMillis();

			final List<ClassDef> v = new ArrayList<>(intermediateCodeGenerator.generate(lambdaForm));
			final List<String> oc = new ArrayList<String>(v.size());
			final List<byte[]> classBytes = new ArrayList<>(v.size());

			// change all of the ClassWriters into byte arrays
			for (final ClassDef classDef : v) {
				final byte[] byteArray = classDef.getClassWriter().toByteArray();
				final ClassReader cr = new ClassReader(byteArray);

				final CheckClassAdapter cca = new CheckClassAdapter(new ClassWriter(0), false);
				cr.accept(cca, ClassReader.SKIP_DEBUG + ClassReader.SKIP_FRAMES);
				classBytes.add(byteArray);
				oc.add(classDef.getName());
			}

			final Transformer transformer = TransformerFactory.newInstance().newTransformer();

			transformer.setOutputProperty("indent", "yes");
			final StreamResult result = new StreamResult(new StringWriter());
			final String xmlString = result.getWriter().toString();
			final String xmlFileName = oc.get(0) + ".xml";

			final long icgTime = System.currentTimeMillis() - baseTime;

			baseTime = System.currentTimeMillis();

			final Iterator<byte[]> iterator = classBytes.iterator();

			JarOutputStream jar = null;
			final File outputFileFile = new File(outputFile);

			// now we make a temp file to hold the results. Then we rename the temp to
			// the desired file name. This avoids problems when the system has loaded
			// a jar file, compiles the source again, and reloads with the new data.

			final File tmpFile = File.createTempFile("TMP_JAR_" + System.currentTimeMillis(), ".jar");
			//Get first class to make Main-Class Attribute
			if (iterator.hasNext()) {
				final Manifest mani = new Manifest();
				mani.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
				mani.getMainAttributes().put(Attributes.Name.MAIN_CLASS, oc.get(0));

				final FileOutputStream fileStream = new FileOutputStream(tmpFile, false);
				jar = new JarOutputStream(fileStream, mani);
			}
			JarEntry entry;
			int nameCounter = 0;
			while (iterator.hasNext()) {
				name = oc.get(nameCounter) + ".class";
				entry = new JarEntry(name);
				jar.putNextEntry(entry);
				jar.write(iterator.next());
				jar.closeEntry();
				nameCounter++;
			}

			entry = new JarEntry(xmlFileName);
			jar.putNextEntry(entry);
			jar.write(xmlString.getBytes());
			jar.closeEntry();
			jar.close();

			if (outputFileFile.exists()) {
				LOGGER.info("Closing and deleting {}", outputFileFile);
				new JarFile(outputFileFile).close();
				outputFileFile.delete();
			}

			if (!tmpFile.renameTo(outputFileFile)) {

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

			final long jarTime = System.currentTimeMillis() - baseTime;
			LOGGER.info("; Total compilation time: {} ms\n", System.currentTimeMillis() - duration);
			LOGGER.info("; Details:\n;  SA: {} ms\n;  ICG: {} ms\n;  Jar: {} ms\n", saTime, icgTime, jarTime);

			// returns 3 values
			final Object[] ret = new Object[3];
			ret[0] = PathnameStruct.buildPathname(outputFile);
			ret[1] = NullStruct.INSTANCE;
			ret[2] = NullStruct.INSTANCE;
			return ret;

		} catch (final FileNotFoundException e) {
			LOGGER.error("FileNotFoundException", e);
			throw new RuntimeException("File " + input + " does not exist.");
		} catch (final IOException e) {
			LOGGER.error("IOException", e);
			throw new RuntimeException("Unable to view contents of File " + input);
		} catch (final IllegalArgumentException e) {
			LOGGER.error("IllegalArgumentException", e);
			throw new RuntimeException(e);
		} catch (final TransformerConfigurationException e) {
			LOGGER.error("TransformerConfigurationException", e);
			throw new RuntimeException(e);
		} catch (final URISyntaxException e) {
			LOGGER.error("URISyntaxException", e);
			throw new RuntimeException(e);
		} finally {
			PackageVariables.PACKAGE.setValue(oldPackageVarValue);
		}
	}

	private LispStruct processTopLevelForm(final LispStruct theForm) {
		LispStruct theRealForm = theForm;

		// See if the first element of the list is a symbol
		if (theRealForm instanceof ListStruct) {
			final ListStruct form = (ListStruct) theRealForm;
			final Object car = form.getFirst();

			if (car instanceof SymbolStruct) {
				// see if that symbol is a macro function and evaluate it if so
				if (((SymbolStruct) car).getFunction() instanceof MacroFunctionExpander) {
					final MacroExpandResult macroExpandReturn = macroExpand.macroExpand(form, Environment.NULL);
					theRealForm = processTopLevelForm(macroExpandReturn.getExpandedForm());
				} else if (car == SpecialOperatorStruct.PROGN) {
					ListStruct resultForms = NullStruct.INSTANCE;
					ListStruct forms = form.getRest();
					while (forms != NullStruct.INSTANCE) {
						if (forms.getFirst() instanceof ListStruct) {
							resultForms = new ConsStruct(processTopLevelForm(forms.getFirst()), resultForms);
						}
						forms = forms.getRest();
					}
					theRealForm = reverse(resultForms);
				} else if ((car == SpecialOperatorStruct.LOCALLY)
						|| (car == SpecialOperatorStruct.MACROLET)
						|| (car == SpecialOperatorStruct.SYMBOL_MACROLET)) {
					// these get handled as top-level forms but with the
					// current lexical bindings
					theRealForm = ListStruct.buildProperList(form);
				} else if (car == SpecialOperatorStruct.EVAL_WHEN) {
					// processEvalWhen...
					final LispStruct evalForm = evalFunction.apply(form);

					theRealForm = processTopLevelForm(evalForm);
				} else {
					evalFunction.apply(form);
				}
			}
		}
		return theRealForm;
	}



























	private List<List<LispStruct>> readForms(final FileStreamStruct file) {
		final List<LispStruct> forms = new ArrayList<>();
		final List<LispStruct> lineNumber = new ArrayList<>();

		final Reader reader = context.getBean(Reader.class, file);

		lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.filePosition(null))));

		// each read creates a new top-level form
		final LispStruct eofValue = null;
		LispStruct form;
		while ((form = reader.read(false, eofValue, true)) != eofValue) {

			// First find out whether we're dealing with a list.
			if (form instanceof ListStruct) {
				form = processTopLevelForm(form);  // may return list of Object[]
			} else {
				LOGGER.warn("Deleted a non-ListStruct form {} found at line {}", form, file.filePosition(null));
				form = NullStruct.INSTANCE;
			}

			if (form != NullStruct.INSTANCE) {
				// might be a vector
				if (form instanceof ValuesStruct) {
					final ValuesStruct arrayForm = (ValuesStruct) form;
					for (int index = 0; index < arrayForm.getValuesList().size(); index++) {
						forms.add(arrayForm.getValuesList().get(index));
						lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.filePosition(null))));
					}
				} else {
					forms.add(form);
					lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.filePosition(null))));
				}
			}
		}
		final List<List<LispStruct>> value = new ArrayList<>();
		value.add(forms);
		value.add(lineNumber);
		return value;
	}

	private static ListStruct reverse(final ListStruct arg1) {

		ListStruct theList = arg1;
		ListStruct newList = NullStruct.INSTANCE;
		while (!theList.equals(NullStruct.INSTANCE)) {
			newList = new ConsStruct(theList.getFirst(), newList);
			theList = theList.getRest();
		}
		return newList;
	}


// TODO: Need: 'translate-logical-pathname' | 'make-pathname' | 'pathname' | 'open'


/*
(defun compile-file (source &key (output-file t)
							     (error-file nil)
							     (trace-file nil)
							     (error-output t)
							     (load nil)
							     (external-format :default)
						         (decoding-error t)
							     ((:verbose *compile-verbose*) *compile-verbose*)
							     ((:print *compile-print*) *compile-print*)
							     ((:progress *compile-progress*) *compile-progress*)
							     ((:block-compile *block-compile-argument*) *block-compile-default*)
							     ((:entry-points *entry-points*) nil)
							     ((:byte-compile *byte-compile*) *byte-compile-default*)
						         ((:xref *record-xref-info*) *record-xref-info*))

  (let* ((jar-file nil)
		 (error-file-stream nil)
		 (output-file-pathname nil)
		 (*compiler-error-output* *compiler-error-output*)
		 (*compiler-trace-output* nil)
		 (compile-won nil)
		 (error-severity nil)
		 (source (verify-source-files source))
		 (source-info (make-file-source-info source external-format decoding-error))
		 (default (pathname (first source))))
    (unwind-protect
	  (progn
		(flet ((frob (file type)
			     (if (eq file t)
			         (make-pathname type type :defaults (translate-logical-pathname default))
			       (pathname file))))

		  (when output-file
		    (setq output-file-pathname
			      (translate-logical-pathname (if (eq output-file t)
												  (compile-file-pathname (first source) :byte-compile *byte-compile*)
												(compile-file-pathname (first source) :output-file output-file :byte-compile *byte-compile*))))
		    (setq jar-file (open-jar-file output-file-pathname (namestring (first source)) (eq *byte-compile* t))))

		  (when trace-file
		    (setq *compiler-trace-output*
			      (open (frob trace-file "trace") :if-exists :supersede :direction :output :external-format external-format)))

		  (when error-file
		    (setq error-file-stream (open (frob error-file "err") :if-exists :supersede :direction :output :external-format external-format))))

		  (setq *compiler-error-output*
			(apply #'make-broadcast-stream
			       (remove nil (list (if (eq error-output t)
										 *error-output*
									   error-output)
									 error-file-stream))))

		(when *compile-verbose*
		  (start-error-output source-info))
		(setq error-severity
			  (let ((*compile-object* jar-file))
			    (sub-compile-file source-info)))
		(setq compile-won t))

	  (close-source-info source-info)

	  (when jar-file
		(close-jar-file jar-file (not compile-won))
		(setq output-file-pathname (pathname (jar-file-stream jar-file)))
		(when (and compile-won *compile-verbose*)
		  (compiler-mumble "~2&; ~A written.~%" (namestring output-file-pathname))))

	  (when *compile-verbose*
	    (finish-error-output source-info compile-won))

	  (when error-file-stream
		(let ((name (pathname error-file-stream)))
		  ;;
		  ;; Leave this var pointing to something reasonable in case someone
		  ;; tries to use it before the LET ends, e.g. during the LOAD.
		  (setq *compiler-error-output* *error-output*)
		  (close error-file-stream)
		  (when (and compile-won (not error-severity))
		    (delete-file name))))

	  (when *compiler-trace-output*
		(close *compiler-trace-output*)))

    (when load
	  (unless output-file
		(error "Can't :LOAD with no output file."))
	  (load output-file-pathname :verbose *compile-verbose*))

	(values (if output-file
				;; Hack around filesystem race condition...
				(or (probe-file output-file-pathname)
					output-file-pathname)
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

;;; Sub-Compile-File  --  Internal
;;;
;;;    Read all forms from Info and compile them, with output to Object.  We
;;; return :ERROR, :WARNING, :NOTE or NIL to indicate the most severe kind of
;;; compiler diagnostic emitted.
;;;
(defun sub-compile-file (info &optional d-s-info)
  (declare (type source-info info))
  (with-ir1-namespace
    (let* ((*block-compile* *block-compile-argument*)
		   (start-errors *compiler-error-count*)
		   (start-warnings *compiler-warning-count*)
		   (start-notes *compiler-note-count*)
		   (*package* *package*)
		   (*initial-package* *package*)
		   (*initial-cookie* *default-cookie*)
		   (*initial-interface-cookie* *default-interface-cookie*)
		   (*default-cookie* (copy-cookie *initial-cookie*))
		   (*default-interface-cookie* (copy-cookie *initial-interface-cookie*))
		   (*lexical-environment* (make-null-environment))
		   (*converting-for-interpreter* nil)
		   (*source-info* info)
		   (*user-source-info* d-s-info)
		   (*compile-file-pathname* nil)
		   (*compile-file-truename* nil)
		   (*top-level-lambdas* ())
		   (*pending-top-level-lambdas* ())
		   (*compiler-error-bailout* #'(lambda ()
										 (compiler-mumble "~2&Fatal error, aborting compilation...~%")
										 (return-from sub-compile-file :error)))
		   (*current-path* nil)
		   (*last-source-context* nil)
		   (*last-original-source* nil)
		   (*last-source-form* nil)
		   (*last-format-string* nil)
		   (*last-format-args* nil)
		   (*last-message-count* 0)
		   (*info-environment* (or (backend-info-environment *backend*)
								   *info-environment*))
		   (*gensym-counter* 0)
		   (intl::*default-domain* intl::*default-domain*))
	      (with-debug-counters
			(clear-stuff)
			(with-compilation-unit ()
			  (process-sources info)

			  (finish-block-compilation)
			  (compile-top-level-lambdas () t)
			  (let ((object *compile-object*))
			    (etypecase object
			      (jar-file (jar-dump-source-info info object))
			      (core-object (fix-core-source-info info object d-s-info))
			      (null)))

			  (cond ((> *compiler-error-count* start-errors) :error)
					((> *compiler-warning-count* start-warnings) :warning)
					((> *compiler-note-count* start-notes) :note)
					(t nil)))))))

*/
}
