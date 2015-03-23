/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassReader;
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

/**
 * ICG code created by this compiler consists of 0 or more
 * class definitions that implement lisp.common.type.Function.
 * Each class definition consists of a static section, a section for variables,
 * and a section for methods.
 */
public class NewEmitter {

	private static final Logger LOGGER = LoggerFactory.getLogger(NewEmitter.class);

	//TODO - fix this to be an external property
	private static final int classVersion = 52; //TODO: 52 == Java 8, 51 == Java 7, 50 == Java 6

	private final List<ClassDef> classes = Collections.synchronizedList(new ArrayList<>());

	private final Stack<ClassDef> classStack = new Stack<>();

	private ClassDef currentClass;

/*****************************************************************************/

	public static void checkClass(final ClassDef classDef) {
		checkClass(classDef.getClassWriter().toByteArray());
	}

	private static void checkClass(final byte[] classBytes) {
		final ClassReader cr = new ClassReader(classBytes);
		final CheckClassAdapter cca = new CheckClassAdapter(new EmptyVisitor());
		cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);
	}

	public static void disassemble(final byte[] classBytes) {
		disassemble(classBytes, new PrintWriter(System.out));
	}

	private static void disassemble(final byte[] classBytes, final PrintWriter out) {
		final ClassReader cr = new ClassReader(classBytes);
		final TraceClassVisitor tcv = new TraceClassVisitor(out);
		cr.accept(tcv, 0); //ClassReader.EXPAND_FRAMES);
	}

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
	 * @param access
	 * 		integer access
	 * @param name
	 * 		String name
	 * @param signature
	 * 		String signature
	 * @param superName
	 * 		String superName
	 * @param interfaces
	 * 		String[] interfaces
	 */
	public void newClass(final int access, final String name, final String signature, final String superName, final String[] interfaces) {
		currentClass = new ClassDef(name, "");
		currentClass.getClassWriter().visit(classVersion, access, name, signature, superName, interfaces);

		classes.add(currentClass);
		classStack.add(currentClass);
	}

	/**
	 * Emitter method for Java visiting the source.
	 *
	 * @param sourceFileName
	 * 		String sourceFileName
	 * @param debug
	 * 		String debug
	 */
	public void visitClassSource(final String sourceFileName, final String debug) {
		currentClass.getClassWriter().visitSource(sourceFileName, debug);
	}

	/**
	 * Emitter method for Java function ADD-OUTER-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param owner
	 * 		String owner
	 * @param name
	 * 		String name
	 * @param desc
	 * 		String desc
	 */
	public void addOuterClass(final String owner, final String name, final String desc) {
		currentClass.getClassWriter().visitOuterClass(owner, name, desc);
	}

	/*
	 *****************
	 * FIELD
	 *****************
	 */

	public void visitClassAttribute(final Attribute attr) {
		currentClass.getClassWriter().visitAttribute(attr);
	}

	/**
	 * Emitter method for Java function ADD-INNER-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name
	 * 		String name
	 * @param outerName
	 * 		String outerName
	 * @param innerName
	 * 		String innerName
	 * @param access
	 * 		Java integer access
	 */
	public void addInnerClass(final String name, final String outerName, final String innerName, final int access) {
		currentClass.getClassWriter().visitInnerClass(name, outerName, innerName, access);
	}

	/**
	 * Emitter method for Java function END-CLASS and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endClass() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endClass with classStack empty");
		}

		currentClass.getClassWriter().visitEnd();

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

	/**
	 * Emitter method for Java function ADD-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param access
	 * 		Java Integer access
	 * @param name
	 * 		String name
	 * @param desc
	 * 		String desc
	 * @param signature
	 * 		String signature
	 * @param value
	 * 		Object value
	 */
	public void newField(final int access, final String name, final String desc, final String signature, final Object value) {
		currentClass.setFieldVisitor(currentClass.getClassWriter().visitField(access, name, desc, signature, value));
	}
	
	/*
	 *****************
	 * ANNOTATION
	 *****************
	 */

	/**
	 * Emitter method for Java function END-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endField() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endField with classStack empty");
		}

		if (currentClass.getFieldVisitor() == null) {
			throw new RuntimeException("Tried to endField with a null field");
		}

		currentClass.getFieldVisitor().visitEnd();
		currentClass.setFieldVisitor(null);
	}

	public void visitFieldAnnotation(final String desc, final boolean visible) {
		currentClass.getFieldVisitor().visitAnnotation(desc, visible);
	}

	public void visitFieldTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.getFieldVisitor().visitTypeAnnotation(typeRef, typePath, desc, visible);
	}

	public void visitFieldAttribute(final Attribute attr) {
		currentClass.getFieldVisitor().visitAttribute(attr);
	}

	/**
	 * Emitter method for Java function NEW-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name
	 * 		String name
	 * @param visible
	 * 		visible
	 */
	public void newAnnotation(final String name, final boolean visible) {
		currentClass.setAnnotationVisitor(currentClass.getClassWriter().visitAnnotation(name, visible));
	}

	/**
	 * Emitter method for Java function NEW-TYPE-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param typeRef
	 * 		typeRef
	 * @param typePath
	 * 		typePath
	 * @param desc
	 * 		desc
	 * @param visible
	 * 		visible
	 */
	public void newTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.setAnnotationVisitor(currentClass.getClassWriter().visitTypeAnnotation(typeRef, typePath, desc, visible));
	}

	/**
	 * Emitter method for Java function END-ANNOTATION and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endAnnotation() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endAnnotation with classStack empty");
		}

		if (currentClass.getAnnotationVisitor() == null) {
			throw new RuntimeException("Tried to endAnnotation with a null annotation");
		}

		currentClass.getAnnotationVisitor().visitEnd();
		currentClass.setAnnotationVisitor(null);
	}

	/*
	 *****************
	 * METHOD
	 *****************
	 */

	/**
	 * Emitter method for Java function EMIT-ANNOTATION-FIELD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name
	 * 		String fieldName
	 * @param value
	 * 		Object fieldValue
	 */
	public void visitAnnotationValue(final String name, final Object value) {
		final String finalFieldValue = value.toString(); // TODO: why are we doing a .toString() here???
		currentClass.getAnnotationVisitor().visit(name, finalFieldValue);
	}

	public void visitAnnotationEnum(final String name, final String desc, final String value) {
		currentClass.getAnnotationVisitor().visitEnum(name, desc, value);
	}

	public void visitAnnotationAnnotation(final String name, final String desc) {
		currentClass.getAnnotationVisitor().visitAnnotation(name, desc);
	}

	public void visitAnnotationArray(final String name) {
		currentClass.getAnnotationVisitor().visitArray(name);
	}

	/**
	 * Emitter method for Java function NEW-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param name
	 * 		String name
	 * @param access
	 * 		Java integer accessFlags
	 * @param paramDesc
	 * 		String paramType
	 * @param returnDesc
	 * 		String returnType
	 * @param signature
	 * 		String signature
	 * @param exceptions
	 * 		String[] exceptions
	 */
	public void newMethod(final int access, final String name, final String paramDesc, final String returnDesc, final String signature, final String[] exceptions) {
		final MethodVisitor mv = currentClass.getClassWriter().visitMethod(access, name, paramDesc + returnDesc, signature, exceptions);
		mv.visitCode();
		currentClass.setMethodVisitor(mv);
	}

	public void visitMethodParameter(final String name, final int access) {
		currentClass.getMethodVisitor().visitParameter(name, access);
	}

	public void visitMethodAnnotationDefault() {
		currentClass.getMethodVisitor().visitAnnotationDefault();
	}

	public void visitMethodAnnotation(final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitAnnotation(desc, visible);
	}

	public void visitMethodTypeAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitTypeAnnotation(typeRef, typePath, desc, visible);
	}

	public void visitMethodParameterAnnotation(final int parameter, final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitParameterAnnotation(parameter, desc, visible);
	}

	public void visitMethodAttribute(final Attribute attr) {
		currentClass.getMethodVisitor().visitAttribute(attr);
	}

	public void visitMethodFrame(final int type, final int nLocal, final Object[] local, final int nStack, final Object[] stack) {
		currentClass.getMethodVisitor().visitFrame(type, nLocal, local, nStack, stack);
	}

	/**
	 * Emitter method for Java function EMIT-LABEL and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param label
	 * 		Label arg1
	 */
	public void visitMethodLabel(final Label label) {
		currentClass.getMethodVisitor().visitLabel(label);
	}

	public void visitMethodInsnAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitInsnAnnotation(typeRef, typePath, desc, visible);
	}

	/**
	 * Emitter method for Java function ADD-CATCH and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param start
	 * 		Label start
	 * @param end
	 * 		Label end
	 * @param handler
	 * 		Label handler
	 * @param type
	 * 		String type
	 */
	public void visitTryCatchBlock(final Label start, final Label end, final Label handler, final String type) {
		currentClass.getMethodVisitor().visitTryCatchBlock(start, end, handler, type);
	}

	public void visitTryCatchAnnotation(final int typeRef, final TypePath typePath, final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitTryCatchAnnotation(typeRef, typePath, desc, visible);
	}

	// -------------------------------------------------------------------------
	// Parameters, annotations and non standard attributes
	// -------------------------------------------------------------------------

/*******************************************************************************
 * The following methods are the emit statments for the individual bytecodes.
 * Each method is followed by its Lisp function needed for the new Lisp
 * compiler to be written.
 *******************************************************************************/

	public void visitLocalVariable(final String name, final String desc, final String signature, final Label start,
	                               final Label end, final int index) {
		currentClass.getMethodVisitor().visitLocalVariable(name, desc, signature, start, end, index);
	}

	public void visitLocalVariableAnnotation(final int typeRef, final TypePath typePath, final Label[] start, final Label[] end,
	                                         final int[] index, final String desc, final boolean visible) {
		currentClass.getMethodVisitor().visitLocalVariableAnnotation(typeRef, typePath, start, end, index, desc, visible);
	}

	public void visitLineNumber(final int line, final Label start) {
		currentClass.getMethodVisitor().visitLineNumber(line, start);
	}

	/**
	 * Emitter method for Java function END-METHOD and class for Lisp
	 * function for new Lisp compiler.
	 */
	public void endMethod() {
		if (currentClass == null) {
			throw new RuntimeException("Tried to endMethod with classStack empty");
		}

		if (currentClass.getMethodVisitor() == null) {
			throw new RuntimeException("Tried to endMethod with a null method");
		}

		currentClass.getMethodVisitor().visitMaxs(0, 0);
		currentClass.getMethodVisitor().visitEnd();
		currentClass.setMethodVisitor(null);
	}

	/**
	 * Emitter method for Java opcode AALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.AALOAD);
	}

	/**
	 * Emitter method for Java opcode AASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.AASTORE);
	}

	/**
	 * Emitter method for Java opcode ACONST_NULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAconst_null() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ACONST_NULL);
	}

	/**
	 * Emitter method for Java opcode ALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitAload(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.ALOAD, var);
	}

	/**
	 * Emitter method for Java opcode ANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type
	 * 		Java String
	 */
	public void emitAnewarray(final String type) {
		currentClass.getMethodVisitor().visitTypeInsn(Opcodes.ANEWARRAY, type);
	}

	/**
	 * Emitter method for Java opcode ARETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAreturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ARETURN);

	}

	/**
	 * Emitter method for Java opcode ARRAYLENGTH and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitArraylength() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ARRAYLENGTH);

	}

	/**
	 * Emitter method for Java opcode ASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitAstore(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.ASTORE, var);
	}

	/**
	 * Emitter method for Java opcode ATHROW and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitAthrow() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ATHROW);
	}

	/**
	 * Emitter method for Java opcode BALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitBaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.BALOAD);
	}

	/**
	 * Emitter method for Java opcode BASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitBastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.BASTORE);
	}

	/**
	 * Emitter method for Java opcode BIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand
	 * 		Java Integer
	 */
	public void emitBipush(final int operand) {
		currentClass.getMethodVisitor().visitIntInsn(Opcodes.BIPUSH, operand);
	}

	/**
	 * Emitter method for Java opcode CALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitCaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.CALOAD);
	}

	/**
	 * Emitter method for Java opcode CASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitCastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.CASTORE);
	}

	/**
	 * Emitter method for Java opcode CHECKCAST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type
	 * 		Java String
	 */
	public void emitCheckcast(final String type) {
		currentClass.getMethodVisitor().visitTypeInsn(Opcodes.CHECKCAST, type);
	}

	/**
	 * Emitter method for Java opcode D2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2f() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.D2F);
	}

	/**
	 * Emitter method for Java opcode D2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2i() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.D2I);
	}

	/**
	 * Emitter method for Java opcode D2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitD2l() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.D2L);
	}

	/**
	 * Emitter method for Java opcode DADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDadd() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DADD);
	}

	/**
	 * Emitter method for Java opcode DALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DALOAD);
	}

	/**
	 * Emitter method for Java opcode DASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DASTORE);
	}

	/**
	 * Emitter method for Java opcode DCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDcmpg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DCMPG);
	}

	/**
	 * Emitter method for Java opcode DCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDcmpl() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DCMPL);
	}

	/**
	 * Emitter method for Java opcode DCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1
	 * 		Java integer
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

		currentClass.getMethodVisitor().visitInsn(dConst);
	}

	/**
	 * Emitter method for Java opcode DDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDdiv() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DDIV);
	}

	/**
	 * Emitter method for Java opcode DLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitDload(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.DLOAD, var);
	}

	/**
	 * Emitter method for Java opcode DMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDmul() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DMUL);
	}

	/**
	 * Emitter method for Java opcode DNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDneg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DNEG);
	}

	/**
	 * Emitter method for Java opcode DREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDrem() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DREM);
	}

	/**
	 * Emitter method for Java opcode DRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDreturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DRETURN);
	}

	/**
	 * Emitter method for Java opcode DSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitDstore(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.DSTORE, var);
	}

	/**
	 * Emitter method for Java opcode DSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDsub() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DSUB);
	}

	/**
	 * Emitter method for Java opcode DUP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP);
	}

	/**
	 * Emitter method for Java opcode DUP_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup_x1() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP_X1);
	}

	/**
	 * Emitter method for Java opcode DUP_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup_x2() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP_X2);
	}

	/**
	 * Emitter method for Java opcode DUP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP2);
	}

	/**
	 * Emitter method for Java opcode DUP2_X1 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2_x1() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP2_X1);
	}

	/**
	 * Emitter method for Java opcode DUP2_X2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitDup2_x2() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.DUP2_X2);
	}

	/**
	 * Emitter method for Java opcode F2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2d() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.F2D);
	}

	/**
	 * Emitter method for Java opcode F2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2i() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.F2I);
	}

	/**
	 * Emitter method for Java opcode F2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitF2l() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.F2L);
	}

	/**
	 * Emitter method for Java opcode FADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFadd() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FADD);
	}

	/**
	 * Emitter method for Java opcode FALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FALOAD);
	}

	/**
	 * Emitter method for Java opcode FASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FASTORE);
	}

	/**
	 * Emitter method for Java opcode FCMPG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFcmpg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FCMPG);
	}

	/**
	 * Emitter method for Java opcode FCMPL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFcmpl() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FCMPL);
	}

	/**
	 * Emitter method for Java opcode FCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1
	 * 		Java integer
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

		currentClass.getMethodVisitor().visitInsn(fConst);
	}

	/**
	 * Emitter method for Java opcode FDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFdiv() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FDIV);
	}

	/**
	 * Emitter method for Java opcode FLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java Integer
	 */
	public void emitFload(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.FLOAD, var);
	}

	/**
	 * Emitter method for Java opcode FMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFmul() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FMUL);
	}

	/**
	 * Emitter method for Java opcode FNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFneg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FNEG);

	}

	/**
	 * Emitter method for Java opcode FREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFrem() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FREM);
	}

	/**
	 * Emitter method for Java opcode FRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFreturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FRETURN);
	}

	/**
	 * Emitter method for Java opcode FSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitFstore(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.FSTORE, var);
	}

	/**
	 * Emitter method for Java opcode FSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitFsub() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.FSUB);
	}

	/**
	 * Emitter method for Java opcode GETFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param desc
	 * 		Java String
	 */
	public void emitGetfield(final String owner, final String name, final String desc) {
		currentClass.getMethodVisitor().visitFieldInsn(Opcodes.GETFIELD, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode GETSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param desc
	 * 		Java String
	 */
	public void emitGetstatic(final String owner, final String name, final String desc) {
		currentClass.getMethodVisitor().visitFieldInsn(Opcodes.GETSTATIC, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode GOTO and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitGoto(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.GOTO, label);
	}

	/**
	 * Emitter method for Java opcode I2B and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2b() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2B);
	}

	/**
	 * Emitter method for Java opcode I2C and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2c() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2C);
	}

	/**
	 * Emitter method for Java opcode I2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2d() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2D);

	}

	/**
	 * Emitter method for Java opcode I2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2f() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2F);
	}

	/**
	 * Emitter method for Java opcode I2L and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2l() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2L);
	}

	/**
	 * Emitter method for Java opcode I2S and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitI2s() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.I2S);
	}

	/**
	 * Emitter method for Java opcode IADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIadd() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IADD);
	}

	/**
	 * Emitter method for Java opcode IALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IALOAD);
	}

	/**
	 * Emitter method for Java opcode IAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIand() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IAND);
	}

	/**
	 * Emitter method for Java opcode IASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IASTORE);
	}

	/**
	 * Emitter method for Java opcode ICONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1
	 * 		Java integer
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

		currentClass.getMethodVisitor().visitInsn(iConst);
	}

	/**
	 * Emitter method for Java opcode IDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIdiv() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IDIV);
	}

	/**
	 * Emitter method for Java opcode IF_ACMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_acmpeq(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ACMPEQ, label);
	}

	/**
	 * Emitter method for Java opcode IF_ACMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_acmpne(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ACMPNE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmpeq(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPEQ, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmpge(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPGE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmpgt(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPGT, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmple(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPLE, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmplt(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPLT, label);
	}

	/**
	 * Emitter method for Java opcode IF_ICMPNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIf_icmpne(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IF_ICMPNE, label);
	}

	/**
	 * Emitter method for Java opcode IFEQ and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfeq(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFEQ, label);
	}

	/**
	 * Emitter method for Java opcode IFGE   and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfge(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFGE, label);
	}

	/**
	 * Emitter method for Java opcode IFGT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfgt(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFGT, label);
	}

	/**
	 * Emitter method for Java opcode IFLE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfle(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFLE, label);
	}

	/**
	 * Emitter method for Java opcode IFLT and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIflt(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFLT, label);
	}

	/**
	 * Emitter method for Java opcode IFNE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfne(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFNE, label);
	}

	/**
	 * Emitter method for Java opcode IFNONNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfnonnull(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFNONNULL, label);
	}

	/**
	 * Emitter method for Java opcode IFNULL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitIfnull(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.IFNULL, label);
	}

	/**
	 * Emitter method for Java opcode IINC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 * @param increment
	 * 		Java integer
	 */
	public void emitIinc(final int var, final int increment) {
		currentClass.getMethodVisitor().visitIincInsn(var, increment);
	}

	/**
	 * Emitter method for Java opcode ILOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitIload(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.ILOAD, var);
	}

	/**
	 * Emitter method for Java opcode IMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitImul() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IMUL);
	}

	/**
	 * Emitter method for Java opcode INEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIneg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.INEG);
	}

	/**
	 * Emitter method for Java opcode INSTANCEOF and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type
	 * 		Java String
	 */
	public void emitInstanceof(final String type) {
		currentClass.getMethodVisitor().visitTypeInsn(Opcodes.INSTANCEOF, type);
	}

	/**
	 * Emitter method for Java opcode INVOKEDYNAMIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param name
	 * 		Java String
	 * @param desc
	 * 		Java String
	 * @param bsm
	 * 		Handle object
	 * @param bsmArgs
	 * 		Handle arguments
	 */
	public void emitInvokedynamic(final String name, final String desc, final Handle bsm, final Object... bsmArgs) {
		currentClass.getMethodVisitor().visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
	}

	/**
	 * Emitter method for Java opcode INVOKEINTERFACE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param paramDesc
	 * 		Java String
	 * @param returnDesc
	 * 		Java String
	 * @param itf
	 * 		true if method's owner is an interface
	 */
	public void emitInvokeinterface(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.getMethodVisitor().visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKESPECIAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param paramDesc
	 * 		Java String
	 * @param returnDesc
	 * 		Java String
	 * @param itf
	 * 		true if method's owner is an interface
	 */
	public void emitInvokespecial(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKESTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param paramDesc
	 * 		Java String
	 * @param returnDesc
	 * 		Java String
	 * @param itf
	 * 		true if method's owner is an interface
	 */
	public void emitInvokestatic(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode INVOKEVIRTUAL and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param paramDesc
	 * 		Java String
	 * @param returnDesc
	 * 		Java String
	 * @param itf
	 * 		true if method's owner is an interface
	 */
	public void emitInvokevirtual(final String owner, final String name, final String paramDesc, final String returnDesc, final boolean itf) {
		currentClass.getMethodVisitor().visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, paramDesc + returnDesc, itf);
	}

	/**
	 * Emitter method for Java opcode IOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIor() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IOR);
	}

	/**
	 * Emitter method for Java opcode IREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIrem() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IREM);
	}

	/**
	 * Emitter method for Java opcode IRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIreturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IRETURN);
	}

	/**
	 * Emitter method for Java opcode ISHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIshl() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ISHL);
	}

	/**
	 * Emitter method for Java opcode ISHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIshr() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ISHR);
	}

	/**
	 * Emitter method for Java opcode ISTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitIstore(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.ISTORE, var);
	}

	/**
	 * Emitter method for Java opcode ISUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIsub() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.ISUB);
	}

	/**
	 * Emitter method for Java opcode IUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIushr() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IUSHR);
	}

	/**
	 * Emitter method for Java opcode IXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitIxor() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.IXOR);
	}

	/**
	 * Emitter method for Java opcode JSR and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param label
	 * 		Java Label
	 */
	public void emitJsr(final Label label) {
		currentClass.getMethodVisitor().visitJumpInsn(Opcodes.JSR, label);
	}

	/**
	 * Emitter method for Java opcode L2D and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2d() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.L2D);
	}

	/**
	 * Emitter method for Java opcode L2F and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2f() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.L2F);
	}

	/**
	 * Emitter method for Java opcode L2I and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitL2i() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.L2I);
	}

	/**
	 * Emitter method for Java opcode LADD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLadd() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LADD);
	}

	/**
	 * Emitter method for Java opcode LALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LALOAD);
	}

	/**
	 * Emitter method for Java opcode LAND and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLand() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LAND);
	}

	/**
	 * Emitter method for Java opcode LASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LASTORE);
	}

	/**
	 * Emitter method for Java opcode LCMP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLcmp() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LCMP);
	}

	/**
	 * Emitter method for Java opcode LCONST and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param arg1
	 * 		Java integer
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

		currentClass.getMethodVisitor().visitInsn(lConst);
	}

	/**
	 * Emitter method for Java opcode LDC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param cst
	 * 		Java Object
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
				currentClass.getMethodVisitor().visitInsn(opCode);
			}
		}
		if ((cst instanceof String) ||
				(cst instanceof Integer) ||
				(cst instanceof Float) ||
				(cst instanceof Double) ||
				(cst instanceof Long) ||
				(cst instanceof Type)) {
			currentClass.getMethodVisitor().visitLdcInsn(cst);
		} else {
			throw new RuntimeException("Ldc called with argument " + cst + ", illegal type");
		}
	}

	/**
	 * Emitter method for Java opcode LDIV and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLdiv() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LDIV);
	}

	/**
	 * Emitter method for Java opcode LLOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitLload(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.LLOAD, var);
	}

	/**
	 * Emitter method for Java opcode LMUL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLmul() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LMUL);
	}

	/**
	 * Emitter method for Java opcode LNEG and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLneg() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LNEG);
	}

	/**
	 * Emitter method for Java opcode LOOKUPSWITCH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param dflt
	 * 		Java Label
	 * @param keys
	 * 		Java int[]
	 * @param labels
	 * 		Java Label[]
	 */
	public void emitLookupswitch(final Label dflt, final int[] keys, final Label[] labels) {
		currentClass.getMethodVisitor().visitLookupSwitchInsn(dflt, keys, labels);
	}

	/**
	 * Emitter method for Java opcode LOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLor() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LOR);
	}

	/**
	 * Emitter method for Java opcode LREM and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLrem() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LREM);
	}

	/**
	 * Emitter method for Java opcode LRETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLreturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LRETURN);
	}

	/**
	 * Emitter method for Java opcode LSHL and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLshl() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LSHL);
	}

	/**
	 * Emitter method for Java opcode LSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLshr() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LSHR);
	}

	/**
	 * Emitter method for Java opcode LSTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitLstore(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.LSTORE, var);
	}

	/**
	 * Emitter method for Java opcode LSUB and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLsub() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LSUB);
	}

	/**
	 * Emitter method for Java opcode LUSHR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLushr() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LUSHR);
	}

	/**
	 * Emitter method for Java opcode LXOR and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitLxor() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.LXOR);
	}

	/**
	 * Emitter method for Java opcode MONITORENTER and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitMonitorenter() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.MONITORENTER);
	}

	/**
	 * Emitter method for Java opcode MONITOREXIT and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitMonitorexit() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.MONITOREXIT);
	}

	/**
	 * Emitter method for Java opcode MULTIANEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param desc
	 * 		Java String
	 * @param dims
	 * 		Java Integer
	 */
	public void emitMultianewarray(final String desc, final int dims) {
		currentClass.getMethodVisitor().visitMultiANewArrayInsn(desc, dims);
	}

	/**
	 * Emitter method for Java opcode NEW and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param type
	 * 		Java String
	 */
	public void emitNew(final String type) {
		currentClass.getMethodVisitor().visitTypeInsn(Opcodes.NEW, type);
	}

	/**
	 * Emitter method for Java opcode NEWARRAY and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand
	 * 		Java Integer
	 */
	public void emitNewarray(final int operand) {
		currentClass.getMethodVisitor().visitIntInsn(Opcodes.NEWARRAY, operand);
	}

	/**
	 * Emitter method for Java opcode NOP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitNop() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.NOP);
	}

	/**
	 * Emitter method for Java opcode POP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitPop() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.POP);
	}

	/**
	 * Emitter method for Java opcode POP2 and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitPop2() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.POP2);
	}

	/**
	 * Emitter method for Java opcode PUTFIELD and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param desc
	 * 		Java String
	 */
	public void emitPutfield(final String owner, final String name, final String desc) {
		currentClass.getMethodVisitor().visitFieldInsn(Opcodes.PUTFIELD, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode PUTSTATIC and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param owner
	 * 		Java String
	 * @param name
	 * 		Java String
	 * @param desc
	 * 		Java String
	 */
	public void emitPutstatic(final String owner, final String name, final String desc) {
		currentClass.getMethodVisitor().visitFieldInsn(Opcodes.PUTSTATIC, owner, name, desc);
	}

	/**
	 * Emitter method for Java opcode RET and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param var
	 * 		Java integer
	 */
	public void emitRet(final int var) {
		currentClass.getMethodVisitor().visitVarInsn(Opcodes.RET, var);
	}

	/**
	 * Emitter method for Java opcode RETURN and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitReturn() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.RETURN);
	}

	/**
	 * Emitter method for Java opcode SALOAD and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSaload() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.SALOAD);
	}

/*
******************************************************************************
* Here are a set of static methods that can be useful in creating and
* debugging an assembled class.
******************************************************************************
*/

	/**
	 * Emitter method for Java opcode SASTORE and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSastore() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.SALOAD);
	}

	/**
	 * Emitter method for Java opcode SIPUSH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param operand
	 * 		Java integer
	 */
	public void emitSipush(final int operand) {
		currentClass.getMethodVisitor().visitIntInsn(Opcodes.SIPUSH, operand);
	}

	/**
	 * Emitter method for Java opcode SWAP and Lisp function for new Lisp
	 * compiler to access Java code.
	 */
	public void emitSwap() {
		currentClass.getMethodVisitor().visitInsn(Opcodes.SWAP);
	}

	/**
	 * Emitter method for Java opcode TABLESWITCH and Lisp function for new Lisp
	 * compiler to access Java code.
	 *
	 * @param min
	 * 		Java Integer
	 * @param max
	 * 		Java Integer
	 * @param dflt
	 * 		Java Label
	 * @param labels
	 * 		Java Label[]
	 */
	public void emitTableswitch(final int min, final int max, final Label dflt, final Label[] labels) {
		currentClass.getMethodVisitor().visitTableSwitchInsn(min, max, dflt, labels);
	}

}
