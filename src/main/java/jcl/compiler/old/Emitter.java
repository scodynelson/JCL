package jcl.compiler.old;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.TypePath;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

/**
 * ICG code created by this compiler consists of 0 or more
 * class definitions that implement lisp.common.type.Function.
 * Each class definition consists of a static section, a section for variables,
 * and a section for methods.
 */
public class Emitter {

	private static final Logger LOGGER = LoggerFactory.getLogger(Emitter.class);

	//TODO - fix this to be an external property
	private static int classVersion = 52; //TODO: 52 == Java 8, 51 == Java 7, 50 == Java 6

	public class ClassDef {
		public final ClassWriter cw;
		MethodVisitor mv;
		FieldVisitor fv;
		AnnotationVisitor av;

		public final String name;

		ClassDef(final ClassWriter cw, final String name) {
			this.cw = cw;
			this.name = name;
		}
	}

	private final List<ClassDef> classes = Collections.synchronizedList(new ArrayList<>());
	private final Stack<ClassDef> classStack = new Stack<>();
	private ClassDef currentClass;

/*****************************************************************************/

	/**
	 * Emitter method for Java function GET-CLASSES and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return the vector of all the classes emitted
	 */
	public List<ClassDef> getClasses() {
		return classes;
	}

	/**
	 * Emitter method for Java function NEW-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name       String name
	 * @param access     integer access
	 * @param superName  String superName
	 * @param interfaces String[] interfaces
	 */
	public void newClass(final int access, final String name, final String signature, final String superName, final String[] interfaces) {
		currentClass = new ClassDef(new ClassWriter(ClassWriter.COMPUTE_MAXS), name);
		currentClass.cw.visit(classVersion, access, name, signature, superName, interfaces);

		classes.add(currentClass);
		classStack.add(currentClass);
	}

	/**
	 * Emitter method for Java visiting the source.
	 *
	 * @param sourceFileName String sourceFileName
	 * @param debug          String debug
	 */
	public void visitClassSource(final String sourceFileName, final String debug) {
		currentClass.cw.visitSource(sourceFileName, debug);
	}

	/**
	 * Emitter method for Java function ADD-OUTER-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param owner String owner
	 * @param name  String name
	 * @param desc  String desc
	 */
	public void addOuterClass(final String owner, final String name, final String desc) {
		currentClass.cw.visitOuterClass(owner, name, desc);
	}

	public void visitClassAttribute(final Attribute attr) {
		currentClass.cw.visitAttribute(attr);
	}

	/**
	 * Emitter method for Java function ADD-INNER-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name      String name
	 * @param outerName String outerName
	 * @param innerName String innerName
	 * @param access    Java integer access
	 */
	public void addInnerClass(final String name, final String outerName, final String innerName, final int access) {
		currentClass.cw.visitInnerClass(name, outerName, innerName, access);
	}

	/**
	 * Emitter method for Java function END-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endClass() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endClass with classStack empty");
		}

		currentClass.cw.visitEnd();

		// pop it off the stack
		classStack.remove(classStack.size() - 1);
		if (classStack.isEmpty()) {
			currentClass = null;
		} else {
			currentClass = classStack.lastElement();
		}
	}

	/**
	 * Emitter method for Java function CLASS-STACK-EMPTY and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @return Object isClassStackEmpty
	 */
	public boolean isClassStackEmpty() {
		return classStack.isEmpty();
	}

	/*
	 *****************
	 * FIELD
	 *****************
	 */


	/**
	 * Emitter method for Java function ADD-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param access    Java Integer access
	 * @param name      String name
	 * @param desc      String desc
	 * @param signature String signature
	 * @param value     Object value
	 */
	public void newField(final int access, final String name, final String desc, final String signature, final Object value) {
		currentClass.fv = currentClass.cw.visitField(access, name, desc, signature, value);
	}

	/**
	 * Emitter method for Java function END-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endField() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endField with classStack empty");
		}

		if (currentClass.fv == null) {
			throw new RuntimeException("Tried to endField with a null field");
		}

		currentClass.fv.visitEnd();
		currentClass.fv = null;
	}

	public void visitFieldAnnotation(final String desc, final boolean visible) {
		currentClass.fv.visitAnnotation(desc, visible);
	}

	public void visitFieldTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.fv.visitTypeAnnotation(typeRef, typePath, desc, visible);
	}

	public void visitFieldAttribute(final Attribute attr) {
		currentClass.fv.visitAttribute(attr);
	}
	
	/*
	 *****************
	 * ANNOTATION
	 *****************
	 */

	/**
	 * Emitter method for Java function NEW-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name String name
	 */
	public void newAnnotation(final String name, final boolean visible) {
		currentClass.av = currentClass.cw.visitAnnotation(name, visible);
	}

	/**
	 * Emitter method for Java function NEW-TYPE-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void newTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.av = currentClass.cw.visitTypeAnnotation(typeRef, typePath, desc, visible);
	}

	/**
	 * Emitter method for Java function END-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endAnnotation() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endAnnotation with classStack empty");
		}

		if (currentClass.av == null) {
			throw new RuntimeException("Tried to endAnnotation with a null annotation");
		}

		currentClass.av.visitEnd();
		currentClass.av = null;
	}

	/**
	 * Emitter method for Java function EMIT-ANNOTATION-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name  String fieldName
	 * @param value Object fieldValue
	 */
	public void visitAnnotationValue(final String name, final Object value) {
		final String finalFieldValue = value.toString(); // TODO: why are we doing a .toString() here???
		currentClass.av.visit(name, finalFieldValue);
	}

	public void visitAnnotationEnum(final String name, final String desc, final String value) {
		currentClass.av.visitEnum(name, desc, value);
	}

	public void visitAnnotationAnnotation(final String name, final String desc) {
		currentClass.av.visitAnnotation(name, desc);
	}

	public void visitAnnotationArray(final String name) {
		currentClass.av.visitArray(name);
	}

	/*
	 *****************
	 * METHOD
	 *****************
	 */

	/**
	 * Emitter method for Java function NEW-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name       String name
	 * @param access     Java integer accessFlags
	 * @param paramDesc  String paramType
	 * @param returnDesc String returnType
	 * @param signature  String signature
	 * @param exceptions String[] exceptions
	 */
	public void newMethod(final int access, final String name, final String paramDesc, final String returnDesc, final String signature, final String[] exceptions) {
		final MethodVisitor mv = currentClass.cw.visitMethod(access, name, paramDesc + returnDesc, signature, exceptions);
		mv.visitCode();
		currentClass.mv = mv;
	}

	public void visitMethodParameter(final String name, final int access) {
		currentClass.mv.visitParameter(name, access);
	}

	public void visitMethodAnnotationDefault() {
		currentClass.mv.visitAnnotationDefault();
	}

	public void visitMethodAnnotation(final String desc, final boolean visible) {
		currentClass.mv.visitAnnotation(desc, visible);
	}

	public void visitMethodTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.mv.visitTypeAnnotation(typeRef, typePath, desc, visible);
	}

	public void visitMethodParameterAnnotation(final int parameter, final String desc, final boolean visible) {
		currentClass.mv.visitParameterAnnotation(parameter, desc, visible);
	}

	public void visitMethodAttribute(final Attribute attr) {
		currentClass.mv.visitAttribute(attr);
	}

	public void visitMethodFrame(final int type, final int nLocal, final Object[] local, final int nStack, final Object[] stack) {
		currentClass.mv.visitFrame(type, nLocal, local, nStack, stack);
	}

	/**
	 * Emitter method for Java function EMIT-LABEL and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param label Label arg1
	 */
	public void visitMethodLabel(final Label label) {
		currentClass.mv.visitLabel(label);
	}

	public void visitMethodInsnAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.mv.visitInsnAnnotation(typeRef, typePath, desc, visible);
	}

	/**
	 * Emitter method for Java function ADD-CATCH and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param start   Label start
	 * @param end     Label end
	 * @param handler Label handler
	 * @param type    String type
	 * @return visitTryCatchBlock Object visitTryCatchBlock
	 */
	public void visitTryCatchBlock(final Label start, final Label end, final Label handler, final String type) {
		currentClass.mv.visitTryCatchBlock(start, end, handler, type);
	}

	public void visitTryCatchAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.mv.visitTryCatchAnnotation(typeRef, typePath, desc, visible);
	}

	public void visitLocalVariable(final String name, final String desc, final String signature, final Label start,
	                               final Label end, final int index) {
		currentClass.mv.visitLocalVariable(name, desc, signature, start, end, index);
	}

	public void visitLocalVariableAnnotation(final int typeRef, final TypePath typePath, final Label[] start, final Label[] end,
	                                         final int[] index, final String desc, final boolean visible) {
		currentClass.mv.visitLocalVariableAnnotation(typeRef, typePath, start, end, index, desc, visible);
	}

	public void visitLineNumber(final int line, final Label start) {
		currentClass.mv.visitLineNumber(line, start);
	}

	/**
	 * Emitter method for Java function END-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endMethod() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endMethod with classStack empty");
		}

		if (currentClass.mv == null) {
			throw new RuntimeException("Tried to endMethod with a null method");
		}

		currentClass.mv.visitMaxs(0, 0);
		currentClass.mv.visitEnd();
		currentClass.mv = null;
	}

	// -------------------------------------------------------------------------
	// Parameters, annotations and non standard attributes
	// -------------------------------------------------------------------------

/*******************************************************************************
 * The following methods are the emit statments for the individual bytecodes.
 * Each method is followed by its Lisp function needed for the new Lisp
 * compiler to be written.
 *******************************************************************************/

	/**
	 * Emitter method for Java opcode AALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAaload() {
		currentClass.mv.visitInsn(Opcodes.AALOAD);
	}

	/**
	 * Emitter method for Java opcode AASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAastore() {
		currentClass.mv.visitInsn(Opcodes.AASTORE);
	}

	/**
	 * Emitter method for Java opcode ACONST_NULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAconst_null() {
		currentClass.mv.visitInsn(Opcodes.ACONST_NULL);
	}

	/**
	 * Emitter method for Java opcode ALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitAload(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.ALOAD, var);
	}

	/**
	 * Emitter method for Java opcode ANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public void emitAnewarray(final String type) {
		currentClass.mv.visitTypeInsn(Opcodes.ANEWARRAY, type);
	}

	/**
	 * Emitter method for Java opcode ARETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAreturn() {
		currentClass.mv.visitInsn(Opcodes.ARETURN);

	}

	/**
	 * Emitter method for Java opcode ARRAYLENGTH and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitArraylength() {
		currentClass.mv.visitInsn(Opcodes.ARRAYLENGTH);

	}

	/**
	 * Emitter method for Java opcode ASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 * @return Object emitAstore
	 */
	public void emitAstore(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.ASTORE, var);
	}

	/**
	 * Emitter method for Java opcode ATHROW and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAthrow() {
		currentClass.mv.visitInsn(Opcodes.ATHROW);
	}

	/**
	 * Emitter method for Java opcode BALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitBaload() {
		currentClass.mv.visitInsn(Opcodes.BALOAD);
	}

	/**
	 * Emitter method for Java opcode BASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitBastore() {
		currentClass.mv.visitInsn(Opcodes.BASTORE);
	}

	/**
	 * Emitter method for Java opcode BIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand Java Integer
	 */
	public void emitBipush(final int operand) {
		currentClass.mv.visitIntInsn(Opcodes.BIPUSH, operand);
	}

	/**
	 * Emitter method for Java opcode CALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitCaload() {
		currentClass.mv.visitInsn(Opcodes.CALOAD);
	}

	/**
	 * Emitter method for Java opcode CASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitCastore() {
		currentClass.mv.visitInsn(Opcodes.CASTORE);
	}

	/**
	 * Emitter method for Java opcode CHECKCAST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public void emitCheckcast(final String type) {
		currentClass.mv.visitTypeInsn(Opcodes.CHECKCAST, type);
	}

	/**
	 * Emitter method for Java opcode D2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2f() {
		currentClass.mv.visitInsn(Opcodes.D2F);
	}

	/**
	 * Emitter method for Java opcode D2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2i() {
		currentClass.mv.visitInsn(Opcodes.D2I);
	}

	/**
	 * Emitter method for Java opcode D2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2l() {
		currentClass.mv.visitInsn(Opcodes.D2L);
	}

	/**
	 * Emitter method for Java opcode DADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDadd() {
		currentClass.mv.visitInsn(Opcodes.DADD);
	}

	/**
	 * Emitter method for Java opcode DALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDaload() {
		currentClass.mv.visitInsn(Opcodes.DALOAD);
	}

	/**
	 * Emitter method for Java opcode DASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDastore() {
		currentClass.mv.visitInsn(Opcodes.DASTORE);
	}

	/**
	 * Emitter method for Java opcode DCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDcmpg() {
		currentClass.mv.visitInsn(Opcodes.DCMPG);
	}

	/**
	 * Emitter method for Java opcode DCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDcmpl() {
		currentClass.mv.visitInsn(Opcodes.DCMPL);
	}

	/**
	 * Emitter method for Java opcode DCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public void emitDconst(final int arg1) {
		final int dConst;
		if (arg1 == 0) {
			dConst = Opcodes.DCONST_0;
		} else if (arg1 == 1) {
			dConst = Opcodes.DCONST_1;
		} else {
			throw new RuntimeException("DCONST called with illegal argument " + arg1 + '.');
		}

		currentClass.mv.visitInsn(dConst);
	}

	/**
	 * Emitter method for Java opcode DDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDdiv() {
		currentClass.mv.visitInsn(Opcodes.DDIV);
	}

	/**
	 * Emitter method for Java opcode DLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitDload(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.DLOAD, var);
	}

	/**
	 * Emitter method for Java opcode DMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDmul() {
		currentClass.mv.visitInsn(Opcodes.DMUL);
	}

	/**
	 * Emitter method for Java opcode DNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDneg() {
		currentClass.mv.visitInsn(Opcodes.DNEG);
	}

	/**
	 * Emitter method for Java opcode DREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDrem() {
		currentClass.mv.visitInsn(Opcodes.DREM);
	}

	/**
	 * Emitter method for Java opcode DRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDreturn() {
		currentClass.mv.visitInsn(Opcodes.DRETURN);
	}

	/**
	 * Emitter method for Java opcode DSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitDstore(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.DSTORE, var);
	}

	/**
	 * Emitter method for Java opcode DSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDsub() {
		currentClass.mv.visitInsn(Opcodes.DSUB);
	}

	/**
	 * Emitter method for Java opcode DUP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup() {
		currentClass.mv.visitInsn(Opcodes.DUP);
	}

	/**
	 * Emitter method for Java opcode DUP_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup_x1() {
		currentClass.mv.visitInsn(Opcodes.DUP_X1);
	}

	/**
	 * Emitter method for Java opcode DUP_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup_x2() {
		currentClass.mv.visitInsn(Opcodes.DUP_X2);
	}

	/**
	 * Emitter method for Java opcode DUP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2() {
		currentClass.mv.visitInsn(Opcodes.DUP2);
	}

	/**
	 * Emitter method for Java opcode DUP2_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2_x1() {
		currentClass.mv.visitInsn(Opcodes.DUP2_X1);
	}

	/**
	 * Emitter method for Java opcode DUP2_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2_x2() {
		currentClass.mv.visitInsn(Opcodes.DUP2_X2);
	}

	/**
	 * Emitter method for Java opcode F2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2d() {
		currentClass.mv.visitInsn(Opcodes.F2D);
	}

	/**
	 * Emitter method for Java opcode F2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2i() {
		currentClass.mv.visitInsn(Opcodes.F2I);
	}

	/**
	 * Emitter method for Java opcode F2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2l() {
		currentClass.mv.visitInsn(Opcodes.F2L);
	}

	/**
	 * Emitter method for Java opcode FADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFadd() {
		currentClass.mv.visitInsn(Opcodes.FADD);
	}

	/**
	 * Emitter method for Java opcode FALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFaload() {
		currentClass.mv.visitInsn(Opcodes.FALOAD);
	}

	/**
	 * Emitter method for Java opcode FASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFastore() {
		currentClass.mv.visitInsn(Opcodes.FASTORE);
	}

	/**
	 * Emitter method for Java opcode FCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFcmpg() {
		currentClass.mv.visitInsn(Opcodes.FCMPG);
	}

	/**
	 * Emitter method for Java opcode FCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFcmpl() {
		currentClass.mv.visitInsn(Opcodes.FCMPL);
	}

	/**
	 * Emitter method for Java opcode FCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public void emitFconst(final int arg1) {
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

		currentClass.mv.visitInsn(fConst);
	}

	/**
	 * Emitter method for Java opcode FDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFdiv() {
		currentClass.mv.visitInsn(Opcodes.FDIV);
	}

	/**
	 * Emitter method for Java opcode FLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java Integer
	 */
	public void emitFload(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.FLOAD, var);
	}

	/**
	 * Emitter method for Java opcode FMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFmul() {
		currentClass.mv.visitInsn(Opcodes.FMUL);
	}

	/**
	 * Emitter method for Java opcode FNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFneg() {
		currentClass.mv.visitInsn(Opcodes.FNEG);

	}

	/**
	 * Emitter method for Java opcode FREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFrem() {
		currentClass.mv.visitInsn(Opcodes.FREM);
	}

	/**
	 * Emitter method for Java opcode FRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFreturn() {
		currentClass.mv.visitInsn(Opcodes.FRETURN);
	}

	/**
	 * Emitter method for Java opcode FSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitFstore(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.FSTORE, var);
	}

	/**
	 * Emitter method for Java opcode FSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFsub() {
		currentClass.mv.visitInsn(Opcodes.FSUB);
	}

	/**
	 * Emitter method for Java opcode GETFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 */
	public void emitGetfield(final String owner, final String name, final String desc) {
		currentClass.mv.visitFieldInsn(Opcodes.GETFIELD, owner, name, desc);
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
	public void emitGetstatic(final String owner, final String name, final String desc) {
		currentClass.mv.visitFieldInsn(Opcodes.GETSTATIC, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode GOTO and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 * @return Object emitGoto
	 */
	public void emitGoto(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.GOTO, label);
	}

	/**
	 * Emitter method for Java opcode I2B and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2b() {
		currentClass.mv.visitInsn(Opcodes.I2B);
	}

	/**
	 * Emitter method for Java opcode I2C and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2c() {
		currentClass.mv.visitInsn(Opcodes.I2C);
	}

	/**
	 * Emitter method for Java opcode I2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2d() {
		currentClass.mv.visitInsn(Opcodes.I2D);

	}

	/**
	 * Emitter method for Java opcode I2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2f() {
		currentClass.mv.visitInsn(Opcodes.I2F);
	}

	/**
	 * Emitter method for Java opcode I2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2l() {
		currentClass.mv.visitInsn(Opcodes.I2L);
	}

	/**
	 * Emitter method for Java opcode I2S and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2s() {
		currentClass.mv.visitInsn(Opcodes.I2S);
	}

	/**
	 * Emitter method for Java opcode IADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIadd() {
		currentClass.mv.visitInsn(Opcodes.IADD);
	}

	/**
	 * Emitter method for Java opcode IALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIaload() {
		currentClass.mv.visitInsn(Opcodes.IALOAD);
	}

	/**
	 * Emitter method for Java opcode IAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIand() {
		currentClass.mv.visitInsn(Opcodes.IAND);
	}

	/**
	 * Emitter method for Java opcode IASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIastore() {
		currentClass.mv.visitInsn(Opcodes.IASTORE);
	}

	/**
	 * Emitter method for Java opcode ICONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public void emitIconst(final int arg1) {
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

		currentClass.mv.visitInsn(iConst);
	}

	/**
	 * Emitter method for Java opcode IDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIdiv() {
		currentClass.mv.visitInsn(Opcodes.IDIV);
	}

	/**
	 * Emitter method for Java opcode IF_ACMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_acmpeq(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ACMPEQ, label);
	}

	/**
	 * Emitter method for Java opcode IF_ACMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_acmpne(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ACMPNE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmpeq(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPEQ, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmpge(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPGE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmpgt(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPGT, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmple(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPLE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmplt(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPLT, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIf_icmpne(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IF_ICMPNE, label);
	}

	/**
	 * Emitter method for Java opcode IFEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfeq(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFEQ, label);
	}

	/**
	 * Emitter method for Java opcode IFGE   and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfge(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFGE, label);
	}

	/**
	 * Emitter method for Java opcode IFGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfgt(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFGT, label);
	}

	/**
	 * Emitter method for Java opcode IFLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfle(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFLE, label);
	}

	/**
	 * Emitter method for Java opcode IFLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIflt(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFLT, label);
	}

	/**
	 * Emitter method for Java opcode IFNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfne(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFNE, label);
	}

	/**
	 * Emitter method for Java opcode IFNONNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfnonnull(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFNONNULL, label);
	}

	/**
	 * Emitter method for Java opcode IFNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitIfnull(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.IFNULL, label);
	}

	/**
	 * Emitter method for Java opcode IINC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var       Java integer
	 * @param increment Java integer
	 */
	public void emitIinc(final int var, final int increment) {
		currentClass.mv.visitIincInsn(var, increment);
	}

	/**
	 * Emitter method for Java opcode ILOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitIload(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.ILOAD, var);
	}

	/**
	 * Emitter method for Java opcode IMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitImul() {
		currentClass.mv.visitInsn(Opcodes.IMUL);
	}

	/**
	 * Emitter method for Java opcode INEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIneg() {
		currentClass.mv.visitInsn(Opcodes.INEG);
	}

	/**
	 * Emitter method for Java opcode INSTANCEOF and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public void emitInstanceof(final String type) {
		currentClass.mv.visitTypeInsn(Opcodes.INSTANCEOF, type);
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
	public void emitInvokedynamic(final String name, final String desc, final Handle bsm, final Object... bsmArgs) {
		currentClass.mv.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
	}

	/**
	 * Emitter method for Java opcode INVOKEINTERFACE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner      Java String
	 * @param name       Java String
	 * @param paramDesc  Java String
	 * @param returnDesc Java String
	 * @param itf        true if method's owner is an interface
	 */
	public void emitInvokeinterface(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKESPECIAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner      Java String
	 * @param name       Java String
	 * @param paramDesc  Java String
	 * @param returnDesc Java String
	 * @param itf        true if method's owner is an interface
	 */
	public void emitInvokespecial(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKESTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner      Java String
	 * @param name       Java String
	 * @param paramDesc  Java String
	 * @param returnDesc Java String
	 * @param itf        true if method's owner is an interface
	 */
	public void emitInvokestatic(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.mv.visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKEVIRTUAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner      Java String
	 * @param name       Java String
	 * @param paramDesc  Java String
	 * @param returnDesc Java String
	 * @param itf        true if method's owner is an interface
	 */
	public void emitInvokevirtual(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode IOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIor() {
		currentClass.mv.visitInsn(Opcodes.IOR);
	}

	/**
	 * Emitter method for Java opcode IREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIrem() {
		currentClass.mv.visitInsn(Opcodes.IREM);
	}

	/**
	 * Emitter method for Java opcode IRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIreturn() {
		currentClass.mv.visitInsn(Opcodes.IRETURN);
	}

	/**
	 * Emitter method for Java opcode ISHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIshl() {
		currentClass.mv.visitInsn(Opcodes.ISHL);
	}

	/**
	 * Emitter method for Java opcode ISHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIshr() {
		currentClass.mv.visitInsn(Opcodes.ISHR);
	}

	/**
	 * Emitter method for Java opcode ISTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitIstore(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.ISTORE, var);
	}

	/**
	 * Emitter method for Java opcode ISUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIsub() {
		currentClass.mv.visitInsn(Opcodes.ISUB);
	}

	/**
	 * Emitter method for Java opcode IUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIushr() {
		currentClass.mv.visitInsn(Opcodes.IUSHR);
	}

	/**
	 * Emitter method for Java opcode IXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIxor() {
		currentClass.mv.visitInsn(Opcodes.IXOR);
	}

	/**
	 * Emitter method for Java opcode JSR and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label Java Label
	 */
	public void emitJsr(final Label label) {
		currentClass.mv.visitJumpInsn(Opcodes.JSR, label);
	}

	/**
	 * Emitter method for Java opcode L2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2d() {
		currentClass.mv.visitInsn(Opcodes.L2D);
	}

	/**
	 * Emitter method for Java opcode L2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2f() {
		currentClass.mv.visitInsn(Opcodes.L2F);
	}

	/**
	 * Emitter method for Java opcode L2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2i() {
		currentClass.mv.visitInsn(Opcodes.L2I);
	}

	/**
	 * Emitter method for Java opcode LADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLadd() {
		currentClass.mv.visitInsn(Opcodes.LADD);
	}

	/**
	 * Emitter method for Java opcode LALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLaload() {
		currentClass.mv.visitInsn(Opcodes.LALOAD);
	}

	/**
	 * Emitter method for Java opcode LAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLand() {
		currentClass.mv.visitInsn(Opcodes.LAND);
	}

	/**
	 * Emitter method for Java opcode LASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLastore() {
		currentClass.mv.visitInsn(Opcodes.LASTORE);
	}

	/**
	 * Emitter method for Java opcode LCMP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLcmp() {
		currentClass.mv.visitInsn(Opcodes.LCMP);
	}

	/**
	 * Emitter method for Java opcode LCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1 Java integer
	 */
	public void emitLconst(final int arg1) {
		final int lConst;
		if (arg1 == 0) {
			lConst = Opcodes.LCONST_0;
		} else if (arg1 == 1) {
			lConst = Opcodes.LCONST_1;
		} else {
			throw new RuntimeException("LCONST called with illegal argument " + arg1 + '.');
		}

		currentClass.mv.visitInsn(lConst);
	}

	/**
	 * Emitter method for Java opcode LDC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param cst Java Object
	 */
	public void emitLdc(final Object cst) {
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
				currentClass.mv.visitInsn(opCode);
			}
		}
		if ((cst instanceof String) ||
				(cst instanceof Integer) ||
				(cst instanceof Float) ||
				(cst instanceof Double) ||
				(cst instanceof Long) ||
				(cst instanceof Type)) {
			currentClass.mv.visitLdcInsn(cst);
		} else {
			throw new RuntimeException("Ldc called with argument " + cst + ", illegal type");
		}
	}

	/**
	 * Emitter method for Java opcode LDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLdiv() {
		currentClass.mv.visitInsn(Opcodes.LDIV);
	}

	/**
	 * Emitter method for Java opcode LLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitLload(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.LLOAD, var);
	}

	/**
	 * Emitter method for Java opcode LMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLmul() {
		currentClass.mv.visitInsn(Opcodes.LMUL);
	}

	/**
	 * Emitter method for Java opcode LNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLneg() {
		currentClass.mv.visitInsn(Opcodes.LNEG);
	}

	/**
	 * Emitter method for Java opcode LOOKUPSWITCH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param dflt   Java Label
	 * @param keys   Java int[]
	 * @param labels Java Label[]
	 */
	public void emitLookupswitch(final Label dflt, final int[] keys, final Label[] labels) {
		currentClass.mv.visitLookupSwitchInsn(dflt, keys, labels);
	}

	/**
	 * Emitter method for Java opcode LOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLor() {
		currentClass.mv.visitInsn(Opcodes.LOR);
	}

	/**
	 * Emitter method for Java opcode LREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLrem() {
		currentClass.mv.visitInsn(Opcodes.LREM);
	}

	/**
	 * Emitter method for Java opcode LRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLreturn() {
		currentClass.mv.visitInsn(Opcodes.LRETURN);
	}

	/**
	 * Emitter method for Java opcode LSHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLshl() {
		currentClass.mv.visitInsn(Opcodes.LSHL);
	}

	/**
	 * Emitter method for Java opcode LSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLshr() {
		currentClass.mv.visitInsn(Opcodes.LSHR);
	}

	/**
	 * Emitter method for Java opcode LSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitLstore(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.LSTORE, var);
	}

	/**
	 * Emitter method for Java opcode LSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLsub() {
		currentClass.mv.visitInsn(Opcodes.LSUB);
	}

	/**
	 * Emitter method for Java opcode LUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLushr() {
		currentClass.mv.visitInsn(Opcodes.LUSHR);
	}

	/**
	 * Emitter method for Java opcode LXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLxor() {
		currentClass.mv.visitInsn(Opcodes.LXOR);
	}

	/**
	 * Emitter method for Java opcode MONITORENTER and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitMonitorenter() {
		currentClass.mv.visitInsn(Opcodes.MONITORENTER);
	}

	/**
	 * Emitter method for Java opcode MONITOREXIT and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitMonitorexit() {
		currentClass.mv.visitInsn(Opcodes.MONITOREXIT);
	}

	/**
	 * Emitter method for Java opcode MULTIANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param desc Java String
	 * @param dims Java Integer
	 */
	public void emitMultianewarray(final String desc, final int dims) {
		currentClass.mv.visitMultiANewArrayInsn(desc, dims);
	}

	/**
	 * Emitter method for Java opcode NEW and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type Java String
	 */
	public void emitNew(final String type) {
		currentClass.mv.visitTypeInsn(Opcodes.NEW, type);
	}

	/**
	 * Emitter method for Java opcode NEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand Java Integer
	 */
	public void emitNewarray(final int operand) {
		currentClass.mv.visitIntInsn(Opcodes.NEWARRAY, operand);
	}

	/**
	 * Emitter method for Java opcode NOP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitNop() {
		currentClass.mv.visitInsn(Opcodes.NOP);
	}

	/**
	 * Emitter method for Java opcode POP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitPop() {
		currentClass.mv.visitInsn(Opcodes.POP);
	}

	/**
	 * Emitter method for Java opcode POP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitPop2() {
		currentClass.mv.visitInsn(Opcodes.POP2);
	}

	/**
	 * Emitter method for Java opcode PUTFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 */
	public void emitPutfield(final String owner, final String name, final String desc) {
		currentClass.mv.visitFieldInsn(Opcodes.PUTFIELD, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode PUTSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner Java String
	 * @param name  Java String
	 * @param desc  Java String
	 */
	public void emitPutstatic(final String owner, final String name, final String desc) {
		currentClass.mv.visitFieldInsn(Opcodes.PUTSTATIC, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode RET and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var Java integer
	 */
	public void emitRet(final int var) {
		currentClass.mv.visitVarInsn(Opcodes.RET, var);
	}

	/**
	 * Emitter method for Java opcode RETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitReturn() {
		currentClass.mv.visitInsn(Opcodes.RETURN);
	}

	/**
	 * Emitter method for Java opcode SALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSaload() {
		currentClass.mv.visitInsn(Opcodes.SALOAD);
	}

	/**
	 * Emitter method for Java opcode SASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSastore() {
		currentClass.mv.visitInsn(Opcodes.SALOAD);
	}

	/**
	 * Emitter method for Java opcode SIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand Java integer
	 */
	public void emitSipush(final int operand) {
		currentClass.mv.visitIntInsn(Opcodes.SIPUSH, operand);
	}

	/**
	 * Emitter method for Java opcode SWAP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSwap() {
		currentClass.mv.visitInsn(Opcodes.SWAP);
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
	public void emitTableswitch(final int min, final int max, final Label dflt, final Label[] labels) {
		currentClass.mv.visitTableSwitchInsn(min, max, dflt, labels);
	}

/*
******************************************************************************
* Here are a set of static methods that can be useful in creating and
* debugging an assembled class.
******************************************************************************
*/

	public static void checkClass(final ClassDef classDef) {
		checkClass(classDef.cw.toByteArray());
	}

	public static void checkClass(final byte[] classBytes) {
		ClassReader cr = new ClassReader(classBytes);
		CheckClassAdapter cca = new CheckClassAdapter(new EmptyVisitor());
		cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);
	}

	public static void disassemble(final byte[] classBytes) {
		disassemble(classBytes, new PrintWriter(System.out));
	}

	public static void disassemble(final byte[] classBytes, final PrintWriter out) {
		final ClassReader cr = new ClassReader(classBytes);
		final TraceClassVisitor tcv = new TraceClassVisitor(out);
		cr.accept(tcv, 0); //ClassReader.EXPAND_FRAMES);
	}

}
