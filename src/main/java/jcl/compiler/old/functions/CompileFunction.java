package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.CompilerClassLoader;
import jcl.compiler.old.Emitter;
import jcl.compiler.old.EmptyVisitor;
import jcl.compiler.old.IntermediateCodeGenerator;
import jcl.compiler.old.SemanticAnalyzer;
import jcl.compiler.old.documentation.AnnotationCollector;
import jcl.compiler.old.documentation.DocumentFactory;
import jcl.compiler.old.symbol.VariableOld;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.CheckMethodAdapter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.lang.reflect.Constructor;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

public class CompileFunction {

	public static final CompileFunction FUNCTION = new CompileFunction();

	//This is only here as a temporary place to cache a loader for debug purposes.
	//Remove it when the defstruct :include is solved.
	private static CompilerClassLoader structLoader = null;
	private XCopyTreeFunction copyTree = XCopyTreeFunction.FUNCTION;
	private SemanticAnalyzer sa;
	private IntermediateCodeGenerator icg;
	private CompilerClassLoader cl;
	private boolean bDebug = false;

	/**
	 * Creates a new instance of compiler
	 */
	private CompileFunction() {
	}

	class MethodEmptyVisitor extends EmptyVisitor {

		@Override
		public MethodVisitor visitMethod(int access, String name, String desc,
		                                 String signature, String[] exceptions) {
			MethodVisitor mv = super.visitMethod(access, name, desc, signature, exceptions);
			CheckMethodAdapter cma = new CheckMethodAdapter(mv);
			return cma;
		}
	}

	@SuppressWarnings("unchecked")
	public LispStruct funcall(LispStruct obj) {

		LispStruct lambda = null;
		LispStruct formCopy = NullStruct.INSTANCE;

		Constructor constructor;
		sa = new SemanticAnalyzer();
		icg = new IntermediateCodeGenerator();

		try {
			// formCopy is a copy of the original form to tack onto the function object
			if (formCopy instanceof List) {
				formCopy = copyTree.funcall(formCopy);
			}
			obj = sa.funcall(obj);

			Vector<Emitter.ClassDef> v = (Vector<Emitter.ClassDef>) icg.funcall(obj);
			Vector<String> oc = new Vector<String>(v.size());
			Vector classBytes = new Vector(v.size());

			// *** setup documentation
			DocumentFactory docFactory = new DocumentFactory();
			Document xmlDoc = docFactory.newInstance();
			Element root = xmlDoc.createElement("documentation");
			xmlDoc.appendChild(root);
			// *** end setup of documentation

			// change all of the ClassWriters into byte arrays
			for (Emitter.ClassDef classDef : v) {
				byte[] byteArray = classDef.cw.toByteArray();
				ClassReader cr = new ClassReader(byteArray);
				CheckClassAdapter cca = new CheckClassAdapter(new MethodEmptyVisitor());
				cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);

				//** Create the generated documentation
				insertDocumentation(cr, xmlDoc, root);

				if (VariableOld.CompileTrace.getValue() != null) {
					System.out.println("Printing the class " + classDef.name + '\n');
					CheckClassAdapter.verify(new ClassReader(byteArray), true, new java.io.PrintWriter(System.out));
//		    TraceClassVisitor tcv = new TraceClassVisitor(new PrintWriter(System.out));
//		    cr.accept(tcv, false);
					System.out.println("Done  with class " + classDef.name + '\n');
				}
				classBytes.add(byteArray);
				oc.add(classDef.getName());
			}
			// now load them
			Vector classesLoaded = new Vector(v.size());

			classesLoaded = loadClasses(classBytes, oc);
			Class primaryClass = (Class) classesLoaded.get(0);
			constructor = primaryClass.getConstructor();
			lambda = (LispStruct) constructor.newInstance(new Object[]{});
			// need to update function classes to support storing the form
			// in the lambda class. This is non-trivial. Probably will be
			// creating a special variable that is bound to the form during the
			// compilation process. Then the function has a final static field
			// that is set to the value when the class is initialized. hmmm.
			// that won't work either...
//		} catch (lisp.system.compiler.exceptions.ReturnFromException rfe) {
//			System.out.println("Exception in Compile Caught!!");
//			System.out.println("Return-from -> " + rfe.getBlockName() + ", value: " + rfe.getValue());
//			System.out.println(rfe.toString());
//			rfe.printStackTrace();
		} catch (Exception e) {
			System.out.flush();
			System.out.println("Exception in Compile Caught!!");
			System.out.println(e.toString());
			e.printStackTrace();
		}
		return lambda;
	}

	@SuppressWarnings("unchecked")
	public Vector loadClasses(Vector classBytes,
	                          Vector<String> oc) {
		cl = new CompilerClassLoader();
		Vector classesLoaded = new Vector();

		// Set to true to cache loader which loaded 1st struct iface and
		//do other debug output, etc.
		boolean debugDefstruct = false;

		for (int x = 0; x < classBytes.size(); x++) {

            /* Some debug stuff we can later remove when we fix
             * the :include problem.  It shouldn't affect anything
             * other than defstruct stuff.  This uses 4 private
             * methods added to the end of this file. They also need
             * to be removed when we are done with debugging.
             */
			// START DEBUG SECTION
			if (debugDefstruct) {
				String temp = oc.get(x);
				cl = debugDefstruct(temp, cl);
			}
			// END DEBUG SECTION

			classesLoaded.add(cl.loadClass((byte[]) classBytes.get(x),
					oc.get(x).replace('.', '/')));
		}

        /* Another debug section that can be removed when the :include
         * issue with defstruct is solved.
         */
		if (debugDefstruct) {
			cacheStructLoader(classesLoaded);
		}

		return classesLoaded;
	}

	private void insertDocumentation(ClassReader cr, Document xmlDoc, Element root) {
		AnnotationCollector annCollect = new AnnotationCollector();
		cr.accept(annCollect, 0); //ClassReader.EXPAND_FRAMES);

		Hashtable docInfo = annCollect.getTable();
		Set keys = docInfo.keySet();
		String key = "";
		String value = "";
		Iterator iterator = keys.iterator();

		if (iterator.hasNext()) {
			Element newDocNode = xmlDoc.createElement("docInstance");
			root.appendChild(newDocNode);

			while (iterator.hasNext()) {
				key = (String) iterator.next();
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

		NodeList docInstances = xmlDoc.getElementsByTagName("docInstance");
		ListStruct mainList = VariableOld.DocumentationDom.getValue();

		for (int i = 0; i < docInstances.getLength(); i++) {
			Node instance = docInstances.item(i);
			if (instance != null) {
				NamedNodeMap attributes = instance.getAttributes();
				if (attributes.getLength() > 0) {
					String uidValue = attributes.getNamedItem("docUID").getNodeValue();
					SymbolStruct uid = GlobalPackageStruct.COMPILER.intern(uidValue).getSymbolStruct();
					mainList = new ConsStruct(new StringStruct(instance.toString()), mainList);
					mainList = new ConsStruct(uid, mainList);
				}
			}
		}

		if (mainList != NullStruct.INSTANCE) {
			VariableOld.DocumentationDom.setValue(mainList);
		}
	}

	// Remove this method when :include issue with defstruct is fixed
	// Try to figure out which stuff should use the struct loader
	// cached from the loader that loaded the first struct interface.
	private boolean useStructLoader(String name) {
		if (doNotUse(name)) {
			return false;
		}
		if ((name.contains("StructureClass"))) {
			return true;
		}
		return false;
	}

	// Remove this method when :include issue with defstruct is fixed.
	// Used an entire method in case other stuff needs to be excluded.
	private boolean doNotUse(String name) {
		//Don't know why, but if we don't exclude "GetDefstructSlotsxxxxxxx"
		//from the list of struct related stuff which should use the
		//struct loader then we can't even compile defstruct.lsp successfully.
		if (name.contains("Slots")) {
			return true;
		}

		return false;
	}

	// Remove this method when :include issue with defstruct is fixed
	private CompilerClassLoader debugDefstruct(String name, CompilerClassLoader cl) {
		System.out.println("Loading: " + name);
		if (useStructLoader(name)) {
			if (structLoader == null) {
				structLoader = cl;
			} else {
				cl = structLoader;
			}
		}
		System.out.println("Using: " + cl);
		System.out.println();

		return cl;
	}

	// Remove this method when :include issue with defstruct is fixed
	private void cacheStructLoader(Vector classesLoaded) {
	    /* This is some more debug stuff related to defstruct. Remove the
	     * whole for loop when we solve the :include issue.
         * This caches the loader that loads the first defstruct interface.
         */
		for (int i = 0; i < classesLoaded.size(); i++) {
			Object temp = classesLoaded.get(i);
			Class cls = (Class) temp;
			if (cls.isInterface() && temp.toString().contains("StructureClass")) {
				// now the current value of cl is what I want to hang on to
				structLoader = cl;
			}
		}
	}
}
