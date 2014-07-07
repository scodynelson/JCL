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
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

import java.io.PrintWriter;
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
	private static int classVersion = 52; //TODO: 52 == Java 8, 51 == Java 7, 50 == Java 6

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

	private Vector<ClassDef> classes = new Vector<>();
	private Stack<ClassDef> classStack = new Stack<>();
	private ClassDef currentClass = null;

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
		MethodVisitor mv = currentClass.cw.visitMethod(accessFlags, name, paramType + returnType, null, null);
		mv.visitCode();
		currentClass.setCurrentMethod(mv);
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
				"(Ljava/lang/String;)Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java visiting the source.
	 */
	public Object emitSource(String fileName, String debug) {
		currentClass.cw.visitSource(fileName, debug);
		return currentClass;
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
	 * @param var Java integer
	 */
	public Object emitAload(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ALOAD, var);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public Object emitAnewarray(final String type) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.ANEWARRAY, type);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ARETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAreturn() {
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
	 * @param var Java integer
	 * @return Object emitAstore
	 */
	public Object emitAstore(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ASTORE, var);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ATHROW and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public Object emitAthrow() {
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
	 * @param operand Java Integer
	 */
	public Object emitBipush(final int operand) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.BIPUSH, operand);
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
	 * @param type Java String
	 */
	public Object emitCheckcast(final String type) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.CHECKCAST, type);
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
	public Object emitDconst(final int arg1) {
		final int dConst;
		if (arg1 == 0) {
			dConst = Opcodes.DCONST_0;
		} else if (arg1 == 1) {
			dConst = Opcodes.DCONST_1;
		} else {
			throw new RuntimeException("DCONST called with illegal argument " + arg1 + '.');
		}

		currentClass.getCurrentMethod().visitInsn(dConst);
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
	 * @param var Java integer
	 */
	public Object emitDload(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.DLOAD, var);
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
	 * @param var Java integer
	 */
	public Object emitDstore(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.DSTORE, var);
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
	public Object emitFconst(final int arg1) {
		final int fConst;
		switch (arg1) {
			case 0:
				fConst = Opcodes.FCONST_0;
				break;
			case 1:
				fConst = Opcodes.FCONST_1;
				break;
			case 2:
				fConst = Opcodes.FCONST_2;
				break;
			default:
				throw new RuntimeException("FCONST called with illegal argument " + arg1 + '.');
		}

		currentClass.getCurrentMethod().visitInsn(fConst);
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
	 * @param var Java Integer
	 */
	public Object emitFload(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.FLOAD, var);
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
	 * @param var Java integer
	 */
	public Object emitFstore(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.FSTORE, var);
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
	 * @param desc  Java String
	 */
	public Object emitGetfield(final String owner, final String name, final String desc) {
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.GETFIELD, owner, name, desc);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode GETSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 * @return Object emitGetstatic
	 */
	public Object emitGetstatic(final String owner, final String name, final String desc) {
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.GETSTATIC, owner, name, desc);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode GOTO and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 * @return Object emitGoto
	 */
	public Object emitGoto(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.GOTO, label);
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
	public Object emitIconst(final int arg1) {
		final int iConst;
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
			case 5:
				iConst = Opcodes.ICONST_5;
				break;
			default:
				throw new RuntimeException("ICONST called with illegal argument " + arg1 + '.');
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
	 * @param label Java Label
	 */
	public Object emitIf_acmpeq(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ACMPEQ, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ACMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_acmpne(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ACMPNE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmpeq(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPEQ, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmpge(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPGE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmpgt(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPGT, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmple(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPLE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmplt(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPLT, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIf_icmpne(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IF_ICMPNE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfeq(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFEQ, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFGE   and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfge(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFGE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfgt(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFGT, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfle(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFLE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIflt(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFLT, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfne(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNE, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNONNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfnonnull(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNONNULL, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IFNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public Object emitIfnull(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.IFNULL, label);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode IINC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var       Java integer
	 * @param increment Java integer
	 */
	public Object emitIinc(final int var, final int increment) {
		currentClass.getCurrentMethod().visitIincInsn(var, increment);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode ILOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public Object emitIload(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ILOAD, var);
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
	 * @param type Java String
	 */
	public Object emitInstanceof(final String type) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.INSTANCEOF, type);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKEDYNAMIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param name    Java String
	 * @param desc    Java String
	 * @param bsm     Handle object
	 * @param bsmArgs Handle arguments
	 */
	public Object emitInvokedynamic(final String name, final String desc, final Handle bsm, final Object... bsmArgs) {
		currentClass.getCurrentMethod().visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKEINTERFACE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 * @param itf   true if method's owner is an interface
	 */
	public Object emitInvokeinterface(final String owner, final String name, final String desc, final boolean itf) {
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, desc, itf);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKESPECIAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 * @param itf   true if method's owner is an interface
	 */
	public Object emitInvokespecial(final String owner, final String name, final String desc, final boolean itf) {
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, desc, itf);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKESTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 * @param itf   true if method's owner is an interface
	 */
	public Object emitInvokestatic(final String owner, final String name, final String desc, final boolean itf) {
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, desc, itf);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode INVOKEVIRTUAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 * @param itf   true if method's owner is an interface
	 */
	public Object emitInvokevirtual(final String owner, final String name, final String desc, final boolean itf) {
		currentClass.getCurrentMethod().visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, desc, itf);
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
	 * @param var Java integer
	 */
	public Object emitIstore(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.ISTORE, var);
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
	 * @param label Java Label
	 */
	public Object emitJsr(final Label label) {
		currentClass.getCurrentMethod().visitJumpInsn(Opcodes.JSR, label);
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
	public Object emitLconst(final int arg1) {
		final int lConst;
		if (arg1 == 0) {
			lConst = Opcodes.LCONST_0;
		} else if (arg1 == 1) {
			lConst = Opcodes.LCONST_1;
		} else {
			throw new RuntimeException("LCONST called with illegal argument " + arg1 + '.');
		}

		currentClass.getCurrentMethod().visitInsn(lConst);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode LDC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param cst Java Object
	 */
	public Object emitLdc(final Object cst) {
		// TODO: Need to review what's done here and if it is right
		if (cst instanceof Integer) {
			final int value = (Integer) cst;
			if ((value >= -1) && (value <= 5)) {
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
	 * Emitter method for Java opcode LLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public Object emitLload(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.LLOAD, var);
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
	public Object emitLookupswitch(final Label dflt, final int[] keys, final Label[] labels) {
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
	 * @param var Java integer
	 */
	public Object emitLstore(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.LSTORE, var);
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
	 * Emitter method for Java opcode MULTIANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param desc Java String
	 * @param dims Java Integer
	 */
	public Object emitMultianewarray(final String desc, final int dims) {
		currentClass.getCurrentMethod().visitMultiANewArrayInsn(desc, dims);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode NEW and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public Object emitNew(final String type) {
		currentClass.getCurrentMethod().visitTypeInsn(Opcodes.NEW, type);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode NEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand Java Integer
	 */
	public Object emitNewarray(final int operand) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.NEWARRAY, operand);
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
	 * @param desc  Java String
	 */
	public Object emitPutfield(final String owner, final String name, final String desc) {
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.PUTFIELD, owner, name, desc);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode PUTSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 */
	public Object emitPutstatic(final String owner, final String name, final String desc) {
		currentClass.getCurrentMethod().visitFieldInsn(Opcodes.PUTSTATIC, owner, name, desc);
		return currentClass;
	}

	/**
	 * Emitter method for Java opcode RET and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public Object emitRet(final int var) {
		currentClass.getCurrentMethod().visitVarInsn(Opcodes.RET, var);
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
	 * @param operand Java integer
	 */
	public Object emitSipush(final int operand) {
		currentClass.getCurrentMethod().visitIntInsn(Opcodes.SIPUSH, operand);
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
	 * @param min    Java Integer
	 * @param max    Java Integer
	 * @param dflt   Java Label
	 * @param labels Java Label[]
	 */
	public Object emitTableswitch(final int min, final int max, final Label dflt, final Label[] labels) {
		currentClass.getCurrentMethod().visitTableSwitchInsn(min, max, dflt, labels);
		return currentClass;
	}
}
