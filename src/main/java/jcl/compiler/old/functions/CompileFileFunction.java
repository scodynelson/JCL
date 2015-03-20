package jcl.compiler.old.functions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.CompilerClassLoader;
import jcl.compiler.old.EmptyVisitor;
import jcl.compiler.old.MacroFunctionExpander;
import jcl.compiler.old.documentation.AnnotationCollector;
import jcl.compiler.old.documentation.DocumentFactory;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.pathnames.PathnameStruct;
import jcl.reader.Reader;
import jcl.streams.CharacterStreamStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.util.CheckClassAdapter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class CompileFileFunction {

	public static final CompileFileFunction FUNCTION = new CompileFileFunction();

	// compilation modes
	public static final SymbolStruct<?> COMPILE = GlobalPackageStruct.SYSTEM.intern("%COMPILE").getSymbol();
	public static final SymbolStruct<?> LOAD = GlobalPackageStruct.COMMON_LISP.intern("LOAD").getSymbol();
	public static final SymbolStruct<?> EVAL = GlobalPackageStruct.COMMON_LISP.intern("EVAL").getSymbol();
	public static final SymbolStruct<?> COMPILE_TOPLEVEL = GlobalPackageStruct.KEYWORD.intern("COMPILE-TOPLEVEL").getSymbol();
	public static final SymbolStruct<?> LOAD_TOPLEVEL = GlobalPackageStruct.KEYWORD.intern("LOAD-TOPLEVEL").getSymbol();
	public static final SymbolStruct<?> EXECUTE = GlobalPackageStruct.KEYWORD.intern("EXECUTE").getSymbol();
	private SemanticAnalyzer sa;
	private IntermediateCodeGenerator icg;
	private CompilerClassLoader cl;
	private boolean bDebug = false;
	private Object lambda = null;

	@Autowired
	private ApplicationContext context;

	/**
	 * Creates a new instance of CompileFile
	 */
	private CompileFileFunction() {
		bDebug = Boolean.getBoolean("bDebug");
	}

	@SuppressWarnings("unchecked")
	public Object apply(ListStruct args) {
		Object input = args.getFirst();
		Object output = args.getRest().getFirst();
		Object verbose = args.getRest().getRest().getFirst();
		long duration = System.currentTimeMillis();

		String inputAsString = input.toString();
		boolean correctInputType = inputAsString.endsWith(".lsp") || inputAsString.endsWith(".lisp");

		if (args == NullStruct.INSTANCE) {
			throw new RuntimeException("too few arguments given to COMPILE-FILE:");
		} else if (!correctInputType) {
			throw new RuntimeException("file to compile must be of type .lsp or .lisp");
		}
		final PackageStruct oldPackageVarValue = PackageVariables.PACKAGE.getValue();
		((SymbolStruct) PackageVariables.PACKAGE).setValue(PackageVariables.PACKAGE.getValue());

		try {
			// bind *package* to itself
			String outputFile = null;
			File f = new File(input.toString());

			if (output != NullStruct.INSTANCE) {
				//Need to associate this value with the correct keyword
				outputFile = output.toString();
			} else {
				String temp = f.getAbsolutePath();
				if (inputAsString.endsWith(".lsp")) {
					temp = temp.substring(0, temp.length() - 4);
				} else {
					temp = temp.substring(0, temp.length() - 5);
				}
				outputFile = temp.concat(".jar");
			}

			// get the name and javafy it
			String name = f.getName();
			String nameRegex = "(\\p{Punct}|\\p{Space})+";
			java.util.regex.Pattern namePattern = java.util.regex.Pattern.compile(nameRegex);
			java.util.Scanner scanner = new java.util.Scanner(name).useDelimiter(namePattern);
			StringBuilder sb = new StringBuilder();
			while (scanner.hasNext()) {
				String str = scanner.next();
				sb.append("").append(Character.toUpperCase(str.charAt(0))).append(str.substring(1));
			}
			SymbolStruct<?> newJavaClassName = new SymbolStruct<>(sb.toString());

			PathnameStruct fact = PathnameStruct.buildPathname(f.getPath());
			CharacterStreamStruct stream = (CharacterStreamStruct) OpenFunction.FUNCTION.funcall(ListStruct.buildProperList(new StringStruct(f.toString()), null, null, KeywordOld.NewVersion));
			Vector<Vector<LispStruct>> formsInfo = readForms(stream);
			java.util.List<LispStruct> formsToCompile = (Vector) formsInfo.get(0);
			Vector<LispStruct> formLneNumbers = formsInfo.get(1);

			// Create the wrap-around lambda expression that encloses all of the forms in the file.
			// We have to give it a specific name so it can be loaded by name
			ListStruct formList = ListStruct.buildProperList(formsToCompile);
			ListStruct nameDeclSpec = ListStruct.buildProperList(Declaration.JAVA_CLASS_NAME, newJavaClassName);
//			ListStruct fileDeclSpec = ListStruct.buildProperList(Declaration.SOURCE_FILE, new StringStruct(f.getAbsoluteFile().getName()));
			ListStruct nameDecl = ListStruct.buildProperList(SpecialOperator.DECLARE, nameDeclSpec);
			formList = new ConsStruct(nameDecl, formList);
			formList = new ConsStruct(NullStruct.INSTANCE, formList);
			formList = new ConsStruct(SpecialOperator.LAMBDA, formList);

			SymbolStruct<?> newSA = GlobalPackageStruct.COMMON_LISP.findSymbol("SEMANTIC-ANALYZER").getSymbol();
			sa = context.getBean(SemanticAnalyzer.class);
//            sa = (newSA == NullStruct.INSTANCE) ? new SemanticAnalyzer() : (Function1)newSA.getFunction();
			icg = context.getBean(IntermediateCodeGenerator.class);

			// set up the timers...
			long baseTime;
			long saTime;
			long icgTime;
			long jarTime;

			baseTime = System.currentTimeMillis();
			LambdaStruct lambdaForm = sa.analyze(formList);
			saTime = System.currentTimeMillis() - baseTime;

			baseTime = System.currentTimeMillis();

			Vector<ClassDef> v = new Vector<>(icg.funcall(lambdaForm));
			Vector<String> oc = new Vector<String>(v.size());
			Vector<byte[]> classBytes = new Vector<>(v.size());

			DocumentFactory docFactory = new DocumentFactory();
			Document xmlDoc = docFactory.newInstance();

			Element root = xmlDoc.createElement("documentation");
			xmlDoc.appendChild(root);

			// change all of the ClassWriters into byte arrays
			for (ClassDef classDef : v) {
				byte[] byteArray = classDef.getClassWriter().toByteArray();
				ClassReader cr = new ClassReader(byteArray);

				String key = "";
				String value = "";
				AnnotationCollector annCollect = new AnnotationCollector();
				cr.accept(annCollect, 0); //ClassReader.EXPAND_FRAMES);
				Hashtable<String, Object> docInfo = annCollect.getTable();

				Set<String> keys = docInfo.keySet();
				Iterator<String> iterator = keys.iterator();

				if (iterator.hasNext()) {
					Element newDocNode = xmlDoc.createElement("docInstance");
					xmlDoc.getDocumentElement().appendChild(newDocNode);

					while (iterator.hasNext()) {
						key = iterator.next();
						value = docInfo.get(key).toString();

						if (!"docUID".equals(key)) {
							Element nodeName = xmlDoc.createElement(key);
							newDocNode.appendChild(nodeName);
							Node nodeValue = xmlDoc.createTextNode(value);
							nodeName.appendChild(nodeValue);
						} else {
							newDocNode.setAttribute(key, value);
						}
					}
				}

				if (bDebug) {
					System.out.println("Printing the class " + classDef.getName() + '\n');
					CheckClassAdapter.verify(new ClassReader(byteArray), true, new java.io.PrintWriter(System.out));
//                    TraceClassVisitor tcv = new TraceClassVisitor(new java.io.PrintWriter(System.out));
//                    cr.accept(tcv, false);
					System.out.println("Done  with class " + classDef.getName() + '\n');
				} else {
					CheckClassAdapter cca = new CheckClassAdapter(new EmptyVisitor());
					cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);
				}
				classBytes.add(byteArray);
				oc.add(classDef.getName());
			}
			// now load them
			Vector<?> classesLoaded = new Vector<>(v.size());

			Transformer transformer = TransformerFactory.newInstance().newTransformer();

			transformer.setOutputProperty("indent", "yes");
			StreamResult result = new StreamResult(new StringWriter());
			DOMSource source = new DOMSource(xmlDoc);
			transformer.transform(source, result);
			String xmlString = result.getWriter().toString();
			String xmlFileName = oc.get(0) + ".xml";

			icgTime = System.currentTimeMillis() - baseTime;
//            if (bDebug) {
//                for (int i=0; i<v.size(); i++) {
//                    System.out.println("Class:\n");
//                    System.out.println(v.get(i).toString());
//                    System.out.println("");
//                }
//            }

			baseTime = System.currentTimeMillis();

			Iterator<byte[]> iterator = classBytes.iterator();
			int nameCounter = 0;
			int xmlCounter = 0;

			Object classFile;
			String file;
			JarEntry entry = null;
			java.io.ByteArrayOutputStream baos = null;

			byte[] buf = new byte[1024];
			FileOutputStream fileStream = null;
			JarOutputStream jar = null;
			File outputFileFile = new File(outputFile);

			// now we make a temp file to hold the results. Then we rename the temp to
			// the desired file name. This avoids problems when the system has loaded
			// a jar file, compiles the source again, and reloads with the new data.

			File tmpFile = File.createTempFile("TMP_JAR_" + System.currentTimeMillis(), ".jar");
			//Get first class to make Main-Class Attribute
			if (iterator.hasNext()) {
				Manifest mani = new Manifest();
				mani.getMainAttributes().put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0");
				mani.getMainAttributes().put(java.util.jar.Attributes.Name.MAIN_CLASS, oc.get(0));
				mani.getMainAttributes().putValue("XmlDocumentation", xmlFileName);

				fileStream = new FileOutputStream(tmpFile, false);
				jar = new JarOutputStream(fileStream, mani);
			}
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
				System.out.println("Closing and deleting " + outputFileFile);
				new java.util.jar.JarFile(outputFileFile).close();
				outputFileFile.delete();
			}

			if (!tmpFile.renameTo(outputFileFile)) {

				System.out.println("Warning: Unable to rename temp file: " + tmpFile + ". JAR file not created.");
				System.out.println("\t Attempting to copy then delete temp file instead...");

				// copying file to destination
				FileInputStream srcFile = new FileInputStream(tmpFile);
				FileOutputStream dstFile = new FileOutputStream(outputFileFile);
				byte[] buffer = new byte[1024];
				for (int x = 0; (x = srcFile.read(buffer)) != -1; ) {
					dstFile.write(buffer, 0, x);
				}

				srcFile.close();
				dstFile.close();

				// deleting temp jar file
				if (!tmpFile.delete()) {
					System.out.println("WARNING: Failed to delete temp jar file.");
				}
			}

			jarTime = System.currentTimeMillis() - baseTime;
			System.out.println("; Total compilation time: "
					+ (System.currentTimeMillis() - duration) + " ms\n");
			System.out.println("; Details:\n"
					+ ";  SA: " + saTime + " ms\n"
					+ ";  ICG: " + icgTime + " ms\n"
					+ ";  Jar: " + jarTime + " ms\n");

			// returns 3 values
			Object[] ret = new Object[3];
			ret[0] = PathnameStruct.buildPathname(outputFile);
			ret[1] = NullStruct.INSTANCE;
			ret[2] = NullStruct.INSTANCE;
			return ret;

		} catch (FileNotFoundException e) {
			System.out.println(e.getCause());
			e.printStackTrace();
			throw new RuntimeException("File " + input.toString() + " does not exist.");
		} catch (IOException e) {
			System.out.println(e.getCause());
			e.printStackTrace();
			throw new RuntimeException("Unable to view contents of File " + input.toString());
		} catch (Exception e) {
			System.out.println(e.getCause());
			e.printStackTrace();
			throw new RuntimeException("Exception caught in COMPILE-FILE:");
		} finally {
			((SymbolStruct) PackageVariables.PACKAGE).setValue(oldPackageVarValue);
		}
	}

	public enum ProcessingMode {
		COMPILE_TIME_TOO, NOT_COMPILE_TIME
	}

	private ProcessingMode mode = ProcessingMode.NOT_COMPILE_TIME;

	public enum ProcessingAction {
		PROCESS, EVALUATE, DISCARD
	}

	private ProcessingAction action;

	/* See Common Lisp The Language, 2nd Edition, Section 5.3.3 */
	@SuppressWarnings("unchecked")
	private ListStruct checkSituation(ListStruct situation) {
		mode = ProcessingMode.NOT_COMPILE_TIME;
		// At the end, just return NIL
		if (situation == NullStruct.INSTANCE) {
			return NullStruct.INSTANCE;
		}
		// perhaps the program used the deprecated names
		SymbolStruct<?> car = (SymbolStruct) situation.getFirst();
		if (car == COMPILE) {
			car = COMPILE_TOPLEVEL;
		} else if (car == LOAD) {
			car = LOAD_TOPLEVEL;
		} else if (car == EVAL) {
			car = EXECUTE;
		}
		// do we have something other than the right symbols?
		if ((car == COMPILE_TOPLEVEL)
				|| (car == LOAD_TOPLEVEL)
				|| (car == EXECUTE)) {
			// it's ok so far
			ListStruct situationResult = checkSituation(situation.getRest());
			return new ConsStruct(car, situationResult);
		} else {
			System.err.println("EVAL-WHEN: improper symbol in the situation place. " + car);
			return checkSituation(situation.getRest());
		}
	}

	private ProcessingAction evalAction(ListStruct situation, ProcessingMode currentMode) {
		if (situation.getAsJavaList().contains(LOAD_TOPLEVEL)) {
			return ProcessingAction.PROCESS;
		} else if (situation.getAsJavaList().contains(COMPILE_TOPLEVEL)) {
			return ProcessingAction.EVALUATE;
		} else if (situation.getAsJavaList().contains(EXECUTE)) {
			if (currentMode == ProcessingMode.COMPILE_TIME_TOO) {
				return ProcessingAction.EVALUATE;
			} else if (currentMode == ProcessingMode.NOT_COMPILE_TIME) {
				return ProcessingAction.DISCARD;
			}
		} else {
			return ProcessingAction.DISCARD;
		}
		return null;
	}

	private ProcessingMode evalMode(ListStruct situation, ProcessingMode currentMode) {
		if (!situation.getAsJavaList().contains(LOAD_TOPLEVEL)) {
			// No new modes when no LT true
			return currentMode;
		} else {
			if (situation.getAsJavaList().contains(COMPILE_TOPLEVEL)) {
				// it's known that LT is present, only one line for CT
				return ProcessingMode.COMPILE_TIME_TOO;
			} else {
				// what's last are CTs being No.
				if (!situation.getAsJavaList().contains(EXECUTE)) {
					// no E, change mode to not-compile-time
					return ProcessingMode.NOT_COMPILE_TIME;
				} else {
					// now we have to look at the current mode
					// The E is present for 2 lines.
					// Have to look at the current mode (CTT or NCT)
					// But the changes are to what they are
					return currentMode;
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private LispStruct processTopLevelForm(LispStruct theForm) {
		// See if the first element of the list is a symbol
		if (theForm instanceof ListStruct) {
			ListStruct form = (ListStruct) theForm;
			Object car = form.getFirst();

			if (car instanceof SymbolStruct) {
				// see if that symbol is a macro function and evaluate it if so
				if (((SymbolStruct) car).getFunction() instanceof MacroFunctionExpander) {
					if (((SymbolStruct) car).getFunction() == null) {
					}
//					theForm = processTopLevelForm(MacroExpandFunction.FUNCTION.funcall(form).getExpandedForm()); TODO
				} else if (car == SpecialOperator.PROGN) {
					ListStruct resultForms = NullStruct.INSTANCE;
					ListStruct forms = form.getRest();
					while (forms != NullStruct.INSTANCE) {
						if (forms.getFirst() instanceof ListStruct) {
							resultForms = new ConsStruct(processTopLevelForm(forms.getFirst()), resultForms);
						}
						forms = forms.getRest();
					}
					theForm = ReverseFunction.funcall(resultForms);
				} else if ((car == SpecialOperator.LOCALLY)
						|| (car == SpecialOperator.MACROLET)
						|| (car == SpecialOperator.SYMBOL_MACROLET)) {
					// these get handled as top-level forms but with the
					// current lexical bindings
					theForm = ListStruct.buildProperList(form);
				} else if (car == SpecialOperator.EVAL_WHEN) {
					// processEvalWhen...
					LispStruct evalForm = handleEvalWhen(form);
					theForm = processTopLevelForm(evalForm);
				} else {
					if (mode == ProcessingMode.COMPILE_TIME_TOO) {
						LispStruct formCopy = (XCopyTreeFunction.FUNCTION).funcall(theForm);
						EvalFunction.FUNCTION.funcall(formCopy);
					}
				}
			}
		}
		return theForm;
	}

	@SuppressWarnings("unchecked")
	private LispStruct handleEvalWhen(ListStruct list) {
		// starts with (eval-when theSituation then-the-rest)
		ProcessingMode currentMode = mode;
		try {
			ListStruct situation = (ListStruct) list.getRest().getFirst();
			// first check the situation flags
			situation = checkSituation(situation);

			ProcessingAction action = evalAction(situation, currentMode);
			ProcessingMode mode = evalMode(situation, currentMode);
			currentMode = mode;

			ListStruct formsToEval = list.getRest().getRest();

			// now, handle the actions
			ListStruct resultForms = NullStruct.INSTANCE;
			while (!formsToEval.getAsJavaList().isEmpty()) {
				LispStruct theForm = formsToEval.getFirst();
				if (theForm instanceof ListStruct) {
					if (action == ProcessingAction.DISCARD) {
					} else if (action == ProcessingAction.EVALUATE) {
						// call the EVAL function
						EvalFunction.FUNCTION.funcall(theForm);
						// but the form isn't later compiled for loading
					} else {
						// handle the 2 forms of Process
						if (mode == ProcessingMode.COMPILE_TIME_TOO) {
							// have to evaluate the form
							EvalFunction.FUNCTION.funcall(XCopyTreeFunction.FUNCTION.funcall(theForm));
						} else {
							System.out.println("Oops The mode was " + mode + " and action " + action + "...");
						}
						// now we just return the form for compilation later
						resultForms = new ConsStruct(theForm, resultForms);
					}
				}
				formsToEval = formsToEval.getRest();
			}
			return ReverseFunction.funcall(resultForms);
		} finally {
			mode = currentMode;
		}
	}

	@SuppressWarnings("unchecked")
	private Vector<Vector<LispStruct>> readForms(CharacterStreamStruct file) {
		assert (file != null);
		Vector<LispStruct> forms = new Vector<>();
//		Vector<IntegerStruct> lineNumber = new Vector<IntegerStruct>();

		final Reader reader = context.getBean(Reader.class, file);

		LispStruct eofValue = null;
		LispStruct form = NullStruct.INSTANCE;
		Object car = NullStruct.INSTANCE;
		int formCount = 0;
//		lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.getLineNumber())));

		// each read creates a new top-level form
		while ((form = reader.read(false, eofValue, true)) != eofValue) {
			assert (form != null);
			Object formCopy = null;
			Object result = null;

			// First find out whether we're dealing with a list.
			if (!(form instanceof ListStruct)) {
				System.out.println("Deleted a non-ListStruct form " + form
//						+ " found at line " + file.getLineNumber()
				);
				form = NullStruct.INSTANCE;
			} else {
				form = processTopLevelForm(form);  // may return list of Object[]
				if ((form instanceof ListStruct) && (((ListStruct) form).getFirst() instanceof ListStruct)) {
//					form = ((ListStruct) form).getAsJavaList().toArray();
				}
			}

			if (form != NullStruct.INSTANCE) {
				// might be a vector
//				if (form instanceof Object[]) {
//					Object[] arrayForm = (Object[]) form;
//					for (int index = 0; index < arrayForm.length; index++) {
//						forms.add(arrayForm[index]);
//						lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.getLineNumber())));
//						formCount++;
//					}
//				} else {
				forms.add(form);
//					lineNumber.add(new IntegerStruct(BigInteger.valueOf(file.getLineNumber())));
				formCount++;
//				}
			}
		}
		Vector<Vector<LispStruct>> value = new Vector<>();
		value.add(forms);
//		value.add(lineNumber);
		return value;
	}
}
