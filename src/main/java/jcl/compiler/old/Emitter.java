package jcl.compiler.old;

import jcl.arrays.BitVectorStruct;
import jcl.arrays.StringStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Stack;
import java.util.Vector;

/**
 * ICG code created by this compiler consists of 0 or more
 * class definitions that implement lisp.common.type.Function.
 * Each class definition consists of a static section, a section for variables,
 * and a section for methods.
 *
 * @author james edited by Patrick Nguyen lispified by Alex Endert
 */
public class Emitter {

	//TODO - fix this to be an external property
	private static int classVersion = 50;
	private static Emitter e = new Emitter();

	//to open the property file
	private static final String PROPERTIES_FILENAME = "emitterFunctions.properties";
	//private static Properties nameToFunction = new Properties();

	//public static final EmitterFunctions StdFunctions = new EmitterFunctions();
	//public static final boolean StdFunctionsInitialized = StdFunctions.initialize();

	public class ClassDef {
		public final ClassWriter cw;
		MethodVisitor mv = null;
		String mvName = null;
		FieldVisitor fv = null;
		AnnotationVisitor av = null;

		public final String name;

		ClassDef(ClassWriter cw, String name) {
			this.cw = cw;
			this.name = name;
		}

		public String getName() {
			return name;
		}

		public ClassWriter getClassWriter() {
			return cw;
		}

		/**
		 * @return an array of assembled class bytes. These are suitable for
		 * writing out to a .class file or into a jar file.
		 */
		public byte[] getClassBytes() {
			return cw.toByteArray();
		}

		public MethodVisitor getCurrentMethod() {
			return mv;
		}

		public void setCurrentMethod(MethodVisitor mv) {
			this.mv = mv;
		}

		public FieldVisitor getCurrentField() {
			return fv;
		}

		public void setCurrentField(FieldVisitor fv) {
			this.fv = fv;
		}

		public AnnotationVisitor getCurrentAnnotation() {
			return av;
		}

		public void setCurrentAnnotation(AnnotationVisitor av) {
			this.av = av;
		}
	}

	private Vector<ClassDef> classes = new Vector<ClassDef>();
	private Stack<ClassDef> classStack = new Stack<ClassDef>();
	private ClassDef currentClass = null;

	static {
		// Read properties file.
		Properties properties = new Properties();
		InputStream propertiesInputstream;
		String currentKey;

		try {
			propertiesInputstream = ClassLoader.getSystemResourceAsStream("lisp/common/resources/" + PROPERTIES_FILENAME);
			properties.load(propertiesInputstream);
		} catch (IOException ex) {
			System.out.println("Unable to load Emitter resources: " + ex);
		}

		for (Enumeration en = properties.propertyNames(); en.hasMoreElements(); ) {
			currentKey = (String) en.nextElement();
			try {
				Class.forName(properties.getProperty(currentKey));
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			}
		}
	}

/*
******************************************************************************
* Here are a set of static methods that can be useful in creating and
* debugging an assembled class.
******************************************************************************
*/


	public static void checkClass(ClassDef classDef) {
		checkClass(classDef.cw.toByteArray());
	}

	public static void checkClass(byte[] classBytes) {
		ClassReader cr = new ClassReader(classBytes);
		CheckClassAdapter cca = new CheckClassAdapter(new EmptyVisitor());
		cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);
	}

	// here's a static method you can use to disassemble any Java class
	public static void disassemble(byte[] classBytes) {
		disassemble(classBytes, new PrintWriter(System.out));
	}

	public static void disassemble(byte[] classBytes, PrintWriter out) {
		ClassReader cr = new ClassReader(classBytes);
		TraceClassVisitor tcv = new TraceClassVisitor(out);
		cr.accept(tcv, 0); //ClassReader.EXPAND_FRAMES);
	}

/*****************************************************************************/

	/**
	 * Creates a new instance of Emitter.
	 */
	public Emitter() {
		//vector of names of all functions
		//NAME.forName()
		//lisp.system.compiler.NAME.forName()
//        System.out.println("MakeEmitter symbol: " + MakeEmitter.SYMBOL);


	}

	/**
	 * Emitter method for Java function GET-CLASSES and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return the vector of all the classes emitted
	 */
	public Vector<ClassDef> getClasses() {
		return classes;
	}

	/**
	 * Emitter method for Java function GET-CURRENT-CLASS-NAME and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return string currentClassName
	 */
	public String getCurrentClassName() {
		return currentClass.getName();
	}

	/**
	 * Emitter method for Java function GET-CURRENT-METHOD-NAME and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return string currentMethodName
	 */
	public String getCurrentMethodName() {
		return currentClass.mvName;
//        MethodNode mn = (MethodNode)currentClass.getCurrentMethod();
//        if (mn != null) {
//            return mn.name;
//        } else {
//            return null;
//        }
	}

	/**
	 * Method for toString for Java function for all classes emmitted
	 *
	 * @return str Strings of emitted classes
	 */
	@Override
	public String toString() {
		String str = "";
		for (int i = 0; i < classes.size(); i++) {
			str += classes.get(i) + "\n\n";
		}
		return str;
	}

	int LineNumber = 0;

	/**
	 * Emitter method for Java function NEW-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name       String name
	 * @param access     integer access
	 * @param superName  String superName
	 * @param interfaces String[] interfaces
	 * @return newClass Object newClass
	 */
	public Object newClass(String name, int access, String superName, String[] interfaces) {
		// the name would come in Java lang format foo.bar
		// needs to be foo/bar to compile
		LineNumber = 0;
		currentClass = new ClassDef(new ClassWriter(ClassWriter.COMPUTE_MAXS), name);
		currentClass.cw.visit(classVersion, access, name, null, superName, interfaces);
		classes.add(currentClass);
		classStack.add(currentClass);
		return currentClass;
	}

	/**
	 * Emitter method for Java function NEW-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name        String name
	 * @param accessFlags Java integer accessFlags
	 * @param paramType   String paramType
	 * @param returnType  String returnType
	 * @return newMethod Object newMethod
	 */
	public Object newMethod(String name, int accessFlags, String paramType, String returnType) {
		currentClass.mvName = name;
		//System.out.println("name: " + name);
		//System.out.println("paramType: " + paramType);
		//System.out.println("returnType: " + returnType);
		MethodVisitor mv = currentClass.cw.visitMethod(accessFlags, name, paramType + returnType, null, null);
		mv.visitCode();
		currentClass.setCurrentMethod(mv);
		//System.out.println("CurrentMrthod is set to: " + name);
		return currentClass;
	}

	/**
	 * Emitter method for Java function ADD-INNER-CLASS-REFERENCE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name      String name
	 * @param outerName String outerName
	 * @param innerName String innerName
	 * @param access    Java integer access
	 * @return addInnerClassReference Object addInnerClassReference
	 */
	public Object addInnerClassReference(String name, String outerName, String innerName, int access) {
		currentClass.cw.visitInnerClass(name, outerName, innerName, access);
		return currentClass;
	}

	/**
	 * Emitter method for Java function ADD-OUTER-CLASS-REFERENCE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param owner String owner
	 * @param name  String name
	 * @param desc  String desc
	 * @return addOuterClassReference Object addOuterClassReference
	 */
	public Object addOuterClassReference(String owner, String name, String desc) {
		currentClass.cw.visitOuterClass(owner, name, desc);
		return currentClass;
	}

	/**
	 * Emitter method for Java function ADD-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param access    Java Integer access
	 * @param name      String name
	 * @param desc      String desc
	 * @param signature String signature
	 * @param value     Object value
	 * @return addField Object addField
	 */
	public Object addField(int access, String name, String desc, String signature, Object value) {
		FieldVisitor fv = currentClass.cw.visitField(access, name, desc, signature, value);
		currentClass.setCurrentField(fv);
		return currentClass;
	}

	/**
	 * Emitter method for Java function NEW-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name String name
	 * @return newAnnotation Object newAnnotation
	 */
	public Object newAnnotation(String name) {
		AnnotationVisitor av = currentClass.cw.visitAnnotation(name, true);
		currentClass.setCurrentAnnotation(av);
		return currentClass;
	}

	/**
	 * Emitter method for Java function EMIT-ANNOTATION-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param fieldName  String fieldName
	 * @param fieldValue Object fieldValue
	 */
	public Object emitAnnotationField(String fieldName, Object fieldValue) {
		String finalFieldValue = fieldValue.toString();
		currentClass.getCurrentAnnotation().visit(fieldName, finalFieldValue);
		return currentClass;
	}

	/**
	 * Emitter method for Java function ADD-CATCH and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param start          Label start
	 * @param end            Lebel end
	 * @param handler        Label handler
	 * @param exceptionClass String exceptionClass
	 * @return addCatch Object addCatch
	 */
	public Object addCatch(Label start, Label end, Label handler, String exceptionClass) {
		currentClass.getCurrentMethod().visitTryCatchBlock(start, end, handler, exceptionClass);
		return currentClass;
	}

	/**
	 * Emitter method for Java function END-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 */
	public Object endAnnotation() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endAnnotation with classStack empty");
		} else if (currentClass.av == null) {
			throw new RuntimeException("Tried to endAnnotation with a null annotation");
		} else {
			currentClass.av.visitEnd();
			currentClass.av = null;
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java function END-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 */
	public Object endClass() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endClass with classStack empty");
		} else {
			// close it out
			currentClass.cw.visitEnd();
			// pop it off the stack
			classStack.remove(classStack.size() - 1);
			if (!classStack.isEmpty()) {
				currentClass = classStack.lastElement();
			} else {
				currentClass = null;
			}
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java function END-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 */
	public Object endMethod() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endMethod with classStack empty");
		} else if (currentClass.mv == null) {
			throw new RuntimeException("Tried to endMethod with a null method");
		} else {
			// close it out
			currentClass.mv.visitMaxs(0, 0);
			currentClass.mv.visitEnd();
			currentClass.mv = null;
			currentClass.mvName = null;
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java function CLASS-STACK-EMPTY and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return Object classStackEmpty
	 */
	public boolean classStackEmpty() {
		return classStack.isEmpty();
	}

//    /** emit into the static section of the class? */
//    public void setEmittingStatic(boolean b) {
//        emittingStatic = b;
//    }

/*******************************************************************************
 *  The following emit methods and their Lisp classes are provided for
 *  convenience.
 ******************************************************************************/

	/**
	 * Emitter method for Java function EMIT-NIL and class for Lisp
	 * function for new Lisp compiler.
	 */
	public Object emitNIL() {
		emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java function EMIT-T and class for Lisp
	 * function for new Lisp compiler.
	 */
	public Object emitT() {
		emitGetstatic("lisp/common/type/T", "T", "Llisp/common/type/Symbol;");
		return TStruct.INSTANCE;
	}

	public void emitComment(String arg1) {
		//TODO - think about making this an Annotation
	}

	/**
	 * Emitter method for Java function EMIT-LABEL and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param arg1 Label arg1
	 */
	public Object emitLabel(Label arg1) {
		currentClass.getCurrentMethod().visitLabel(arg1);
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym lisp.common.type.Sybmbol sym
	 */
	public Object emitSymbolPackage(SymbolStruct sym) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			PackageStruct homePkgName = sym.getSymbolPackage();
			if (homePkgName == GlobalPackageStruct.COMMON_LISP) {
				emitGetstatic("lisp/common/type/Package", "CommonLisp", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.COMMON_LISP_USER) {
				emitGetstatic("lisp/common/type/Package", "CommonLispUser", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.KEYWORD) {
				emitGetstatic("lisp/common/type/Package", "Keyword", "Llisp/common/type/Package;");
			} else if (homePkgName == GlobalPackageStruct.SYSTEM) {
				emitGetstatic("lisp/common/type/Package", "System", "Llisp/common/type/Package;");
			} else {
				emitPackage(homePkgName);
			}
		} else {
			// no package
		}
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java function EMIT-PACKAGE and class for Lisp function
	 * for new Lisp compiler.
	 *
	 * @param name lisp.common.type.Package name
	 */
	public Object emitPackage(PackageStruct name) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		emitLdc(name.getName().toString());
		//String owner, String name, String descr
		emitInvokestatic("lisp/system/PackageImpl", "findPackage",
				"(Ljava/lang/String;)Llisp/common/type/Package;");
		return NILStruct.INSTANCE;
	}

/*******************************************************************************
 * The following methods are the emit statments for the individual bytecodes.
 * Each method is followed by its Lisp function needed for the new Lisp
 * compiler to be written.
 *******************************************************************************/

	/**
	 * Emitter method for Java opcode AALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.AALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode AASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.AASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ACONST_NULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAconst_null() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.ACONST_NULL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitAload(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ALOAD, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java String
	 */
	public Object emitAnewarray(String arg1) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.ANEWARRAY, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ARETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAreturn() {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitInsn(Opcodes.ARETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ARRAYLENGTH and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitArraylength() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.ARRAYLENGTH);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 * @return Object emitAstore
	 */
	public Object emitAstore(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ASTORE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ATHROW and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAthrow() {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitInsn(Opcodes.ATHROW);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode BALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitBaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.BALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode BASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitBastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.BASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode BIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Integer
	 */
	public Object emitBipush(int arg1) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.BIPUSH, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode CALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitCaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.CALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode CASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitCastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.CASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode CHECKCAST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java String
	 */
	public Object emitCheckcast(String arg1) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.CHECKCAST, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode D2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitD2f() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.D2F);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode D2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitD2i() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.D2I);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode D2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitD2l() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.D2L);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDadd() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DADD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDcmpg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DCMPG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDcmpl() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DCMPL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitDconst(int arg1) {
		if (arg1 == 0) {
			currentClass.getCurrentMethod().visitInsn(Opcodes.DCONST_0);
		} else if (arg1 == 1) {
			currentClass.getCurrentMethod().visitInsn(Opcodes.DCONST_1);
		} else {
			throw new RuntimeException("dconst called with argument " + arg1 + ", illegal value");
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDdiv() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DDIV);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitDload(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.DLOAD, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDmul() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DMUL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDneg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DNEG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDrem() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DREM);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDreturn() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DRETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitDstore(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.DSTORE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDsub() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DSUB);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup_x1() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP_X1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup_x2() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP_X2);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup2() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP2);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP2_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup2_x1() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP2_X1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode DUP2_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitDup2_x2() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.DUP2_X2);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode F2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitF2d() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.F2D);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode F2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitF2i() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.F2I);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode F2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitF2l() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.F2L);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFadd() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FADD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFcmpg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FCMPG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFcmpl() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FCMPL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitFconst(int arg1) {
		int constVal = -1;
		switch (arg1) {
			case 0:
				constVal = Opcodes.FCONST_0;
				break;
			case 1:
				constVal = Opcodes.FCONST_1;
				break;
			case 2:
				constVal = Opcodes.FCONST_2;
				break;
			default:
				throw new RuntimeException("fconst called with argument " + arg1 + ", illegal value");
		}
		currentClass.getCurrentMethod().visitInsn(constVal);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFdiv() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FDIV);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Integer
	 */
	public Object emitFload(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.FLOAD, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFmul() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FMUL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFneg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FNEG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFrem() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FREM);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFreturn() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FRETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitFstore(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.FSTORE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode FSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitFsub() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.FSUB);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode GETFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitGetfield(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.GETFIELD, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode GETSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 * @return Object emitGetstatic
	 */
	public Object emitGetstatic(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.GETSTATIC, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode GOTO and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 * @return Object emitGoto
	 */
	public Object emitGoto(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.GOTO, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2B and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2b() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2B);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2C and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2c() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2C);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2d() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2D);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2f() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2F);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2l() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2L);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode I2S and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitI2s() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.I2S);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIadd() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IADD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIand() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IAND);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ICONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitIconst(int arg1) {
		int iConst = -2;
		switch (arg1) {
			case -1:
				iConst = Opcodes.ICONST_M1;
				break;
			case 0:
				iConst = Opcodes.ICONST_0;
				break;
			case 1:
				iConst = Opcodes.ICONST_1;
				break;
			case 2:
				iConst = Opcodes.ICONST_2;
				break;
			case 3:
				iConst = Opcodes.ICONST_3;
				break;
			case 4:
				iConst = Opcodes.ICONST_4;
				break;
			default:
				throw new RuntimeException("iconst called with argument " + arg1 + ", illegal value");
		}
		currentClass.getCurrentMethod().visitInsn(iConst);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIdiv() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IDIV);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ACMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_acmpeq(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ACMPEQ, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ACMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_acmpne(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ACMPNE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmpeq(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPEQ, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmpge(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPGE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmpgt(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPGT, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmple(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPLE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmplt(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPLT, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIf_icmpne(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPNE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfeq(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFEQ, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFGE   and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfge(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFGE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfgt(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFGT, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfle(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFLE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIflt(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFLT, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfne(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNONNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfnonnull(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNONNULL, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitIfnull(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNULL, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ILOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitIload(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ILOAD, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitImul() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IMUL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIneg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.INEG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INSTANCEOF and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java String
	 */
	public Object emitInstanceof(String arg1) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.INSTANCEOF, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKEINTERFACE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitInvokeinterface(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKESPECIAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitInvokespecial(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKESTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitInvokestatic(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKEVIRTUAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitInvokevirtual(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIor() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IOR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIrem() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IREM);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIreturn() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IRETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ISHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIshl() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.ISHL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ISHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIshr() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.ISHR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ISTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitIstore(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ISTORE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ISUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIsub() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.ISUB);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIushr() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IUSHR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitIxor() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.IXOR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode JSR and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java Label
	 */
	public Object emitJsr(Label arg1) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.JSR, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode L2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitL2d() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.L2D);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode L2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitL2f() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.L2F);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode L2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitL2i() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.L2I);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLadd() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LADD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLand() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LAND);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LASTORE);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LCMP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLcmp() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LCMP);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitLconst(int arg1) {
		if (arg1 == 0) {
			currentClass.getCurrentMethod().visitInsn(Opcodes.LCONST_0);
		} else if (arg1 == 1) {
			currentClass.getCurrentMethod().visitInsn(Opcodes.LCONST_1);
		} else {
			throw new RuntimeException("lconst called with argument " + arg1 + ", illegal value");
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LDC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param cst Java Object
	 */
	public Object emitLdc(Object cst) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		if (cst instanceof Integer) {
			//System.out.println("I think this is an integer:" + cst);
			int value = ((Integer) cst).intValue();
			if (value >= -1 && value <= 5) {
				int opCode = 0;
				switch (value) {
					case -1:
						opCode = Opcodes.ICONST_M1;
						break;
					case 0:
						opCode = Opcodes.ICONST_0;
						break;
					case 1:
						opCode = Opcodes.ICONST_1;
						break;
					case 2:
						opCode = Opcodes.ICONST_2;
						break;
					case 3:
						opCode = Opcodes.ICONST_3;
						break;
					case 4:
						opCode = Opcodes.ICONST_4;
						break;
					case 5:
						opCode = Opcodes.ICONST_5;
						break;
				}
				currentClass.getCurrentMethod().visitInsn(opCode);
				return NILStruct.INSTANCE;
			}
		}
		if ((cst instanceof String) ||
				(cst instanceof Integer) ||
				(cst instanceof Float) ||
				(cst instanceof Double) ||
				(cst instanceof Long) ||
				(cst instanceof Type)) {
			currentClass.getCurrentMethod().visitLdcInsn(cst);
		} else if (cst instanceof BitVectorStruct) {
			currentClass.getCurrentMethod().visitLdcInsn(cst.toString());
		} else if (cst instanceof StringStruct) {
			currentClass.getCurrentMethod().visitLdcInsn(cst.toString());
		} else {
			throw new RuntimeException("Ldc called with argument " + cst + ", illegal type");
		}
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLdiv() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LDIV);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LINE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param line Java integer
	 * @param name Java Label
	 */
	public Object emitLine(int line, Label name) {
//        currentClass.getCurrentMethod().visitLineNumber(line, name);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitLload(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.LLOAD, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLmul() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LMUL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLneg() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LNEG);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LOOKUPSWITCH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param dflt   Java Label
	 * @param keys   Java int[]
	 * @param labels Java Label[]
	 */
	public Object emitLookupswitch(Label dflt, int[] keys, Label[] labels) {
		currentClass.getCurrentMethod().visitLookupSwitchInsn(dflt, keys, labels);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLor() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LOR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLrem() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LREM);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLreturn() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LRETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LSHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLshl() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LSHL);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLshr() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LSHR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitLstore(int arg1) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.LSTORE, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLsub() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LSUB);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLushr() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LUSHR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitLxor() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.LXOR);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode MONITORENTER and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitMonitorenter() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.MONITORENTER);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode MONITOREXIT and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitMonitorexit() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.MONITOREXIT);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode NEW and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java String
	 */
	public Object emitNew(String arg1) {
		//System.out.println("currentClass: " + currentClass);
		//System.out.println("currentClass.getCurrentMethod():" + currentClass.getCurrentMethod());
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.NEW, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode NEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java String
	 */
	public Object emitNewarray(String arg1) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.NEWARRAY, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode NOP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitNop() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.NOP);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode POP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitPop() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.POP);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode POP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitPop2() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.POP2);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode PUTFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitPutfield(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.PUTFIELD, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode PUTSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param descr Java String
	 */
	public Object emitPutstatic(String owner, String name, String descr) {
//        Label label = new Label();
//        emitLabel(label);
//        emitLine(++LineNumber, label);
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.PUTSTATIC, owner, name, descr);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode RET and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param register Java integer
	 */
	public Object emitRet(int register) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.RET, register);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode RETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitReturn() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.RETURN);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode SALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitSaload() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.SALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode SASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitSastore() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.SALOAD);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode SIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public Object emitSipush(int arg1) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.SIPUSH, arg1);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode SOURCE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitSource(String fileName, String debug) {
		currentClass.cw.visitSource(fileName, debug);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode SWAP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitSwap() {
		currentClass.getCurrentMethod().visitInsn(Opcodes.SWAP);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode TABLESWITCH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param min           Java Integer
	 * @param max           Java Integer
	 * @param dflt          Java Label
	 * @param handlerBlocks Java Label[]
	 */
	public Object emitTableswitch(int min, int max, Label dflt, Label[] handlerBlocks) {
		currentClass.getCurrentMethod().visitTableSwitchInsn(min, max, dflt, handlerBlocks);
		return currentClass;
	}
}
