package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ConstantDynamic;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import static org.objectweb.asm.Opcodes.AALOAD;
import static org.objectweb.asm.Opcodes.AASTORE;
import static org.objectweb.asm.Opcodes.ACONST_NULL;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.ANEWARRAY;
import static org.objectweb.asm.Opcodes.ARETURN;
import static org.objectweb.asm.Opcodes.ARRAYLENGTH;
import static org.objectweb.asm.Opcodes.ASTORE;
import static org.objectweb.asm.Opcodes.ATHROW;
import static org.objectweb.asm.Opcodes.BALOAD;
import static org.objectweb.asm.Opcodes.BASTORE;
import static org.objectweb.asm.Opcodes.BIPUSH;
import static org.objectweb.asm.Opcodes.CALOAD;
import static org.objectweb.asm.Opcodes.CASTORE;
import static org.objectweb.asm.Opcodes.CHECKCAST;
import static org.objectweb.asm.Opcodes.D2F;
import static org.objectweb.asm.Opcodes.D2I;
import static org.objectweb.asm.Opcodes.D2L;
import static org.objectweb.asm.Opcodes.DADD;
import static org.objectweb.asm.Opcodes.DALOAD;
import static org.objectweb.asm.Opcodes.DASTORE;
import static org.objectweb.asm.Opcodes.DCMPG;
import static org.objectweb.asm.Opcodes.DCMPL;
import static org.objectweb.asm.Opcodes.DCONST_0;
import static org.objectweb.asm.Opcodes.DCONST_1;
import static org.objectweb.asm.Opcodes.DDIV;
import static org.objectweb.asm.Opcodes.DLOAD;
import static org.objectweb.asm.Opcodes.DMUL;
import static org.objectweb.asm.Opcodes.DNEG;
import static org.objectweb.asm.Opcodes.DREM;
import static org.objectweb.asm.Opcodes.DRETURN;
import static org.objectweb.asm.Opcodes.DSTORE;
import static org.objectweb.asm.Opcodes.DSUB;
import static org.objectweb.asm.Opcodes.DUP;
import static org.objectweb.asm.Opcodes.DUP2;
import static org.objectweb.asm.Opcodes.DUP2_X1;
import static org.objectweb.asm.Opcodes.DUP2_X2;
import static org.objectweb.asm.Opcodes.DUP_X1;
import static org.objectweb.asm.Opcodes.DUP_X2;
import static org.objectweb.asm.Opcodes.F2D;
import static org.objectweb.asm.Opcodes.F2I;
import static org.objectweb.asm.Opcodes.F2L;
import static org.objectweb.asm.Opcodes.FADD;
import static org.objectweb.asm.Opcodes.FALOAD;
import static org.objectweb.asm.Opcodes.FASTORE;
import static org.objectweb.asm.Opcodes.FCMPG;
import static org.objectweb.asm.Opcodes.FCMPL;
import static org.objectweb.asm.Opcodes.FCONST_0;
import static org.objectweb.asm.Opcodes.FCONST_1;
import static org.objectweb.asm.Opcodes.FCONST_2;
import static org.objectweb.asm.Opcodes.FDIV;
import static org.objectweb.asm.Opcodes.FLOAD;
import static org.objectweb.asm.Opcodes.FMUL;
import static org.objectweb.asm.Opcodes.FNEG;
import static org.objectweb.asm.Opcodes.FREM;
import static org.objectweb.asm.Opcodes.FRETURN;
import static org.objectweb.asm.Opcodes.FSTORE;
import static org.objectweb.asm.Opcodes.FSUB;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.GETSTATIC;
import static org.objectweb.asm.Opcodes.GOTO;
import static org.objectweb.asm.Opcodes.I2B;
import static org.objectweb.asm.Opcodes.I2C;
import static org.objectweb.asm.Opcodes.I2D;
import static org.objectweb.asm.Opcodes.I2F;
import static org.objectweb.asm.Opcodes.I2L;
import static org.objectweb.asm.Opcodes.I2S;
import static org.objectweb.asm.Opcodes.IADD;
import static org.objectweb.asm.Opcodes.IALOAD;
import static org.objectweb.asm.Opcodes.IAND;
import static org.objectweb.asm.Opcodes.IASTORE;
import static org.objectweb.asm.Opcodes.ICONST_0;
import static org.objectweb.asm.Opcodes.ICONST_1;
import static org.objectweb.asm.Opcodes.ICONST_2;
import static org.objectweb.asm.Opcodes.ICONST_3;
import static org.objectweb.asm.Opcodes.ICONST_4;
import static org.objectweb.asm.Opcodes.ICONST_5;
import static org.objectweb.asm.Opcodes.ICONST_M1;
import static org.objectweb.asm.Opcodes.IDIV;
import static org.objectweb.asm.Opcodes.IFEQ;
import static org.objectweb.asm.Opcodes.IFGE;
import static org.objectweb.asm.Opcodes.IFGT;
import static org.objectweb.asm.Opcodes.IFLE;
import static org.objectweb.asm.Opcodes.IFLT;
import static org.objectweb.asm.Opcodes.IFNE;
import static org.objectweb.asm.Opcodes.IFNONNULL;
import static org.objectweb.asm.Opcodes.IFNULL;
import static org.objectweb.asm.Opcodes.IF_ACMPEQ;
import static org.objectweb.asm.Opcodes.IF_ACMPNE;
import static org.objectweb.asm.Opcodes.IF_ICMPEQ;
import static org.objectweb.asm.Opcodes.IF_ICMPGE;
import static org.objectweb.asm.Opcodes.IF_ICMPGT;
import static org.objectweb.asm.Opcodes.IF_ICMPLE;
import static org.objectweb.asm.Opcodes.IF_ICMPLT;
import static org.objectweb.asm.Opcodes.IF_ICMPNE;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.IMUL;
import static org.objectweb.asm.Opcodes.INEG;
import static org.objectweb.asm.Opcodes.INSTANCEOF;
import static org.objectweb.asm.Opcodes.INVOKEINTERFACE;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;
import static org.objectweb.asm.Opcodes.IOR;
import static org.objectweb.asm.Opcodes.IREM;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.ISHL;
import static org.objectweb.asm.Opcodes.ISHR;
import static org.objectweb.asm.Opcodes.ISTORE;
import static org.objectweb.asm.Opcodes.ISUB;
import static org.objectweb.asm.Opcodes.IUSHR;
import static org.objectweb.asm.Opcodes.IXOR;
import static org.objectweb.asm.Opcodes.JSR;
import static org.objectweb.asm.Opcodes.L2D;
import static org.objectweb.asm.Opcodes.L2F;
import static org.objectweb.asm.Opcodes.L2I;
import static org.objectweb.asm.Opcodes.LADD;
import static org.objectweb.asm.Opcodes.LALOAD;
import static org.objectweb.asm.Opcodes.LAND;
import static org.objectweb.asm.Opcodes.LASTORE;
import static org.objectweb.asm.Opcodes.LCMP;
import static org.objectweb.asm.Opcodes.LCONST_0;
import static org.objectweb.asm.Opcodes.LCONST_1;
import static org.objectweb.asm.Opcodes.LDIV;
import static org.objectweb.asm.Opcodes.LLOAD;
import static org.objectweb.asm.Opcodes.LMUL;
import static org.objectweb.asm.Opcodes.LNEG;
import static org.objectweb.asm.Opcodes.LOR;
import static org.objectweb.asm.Opcodes.LREM;
import static org.objectweb.asm.Opcodes.LRETURN;
import static org.objectweb.asm.Opcodes.LSHL;
import static org.objectweb.asm.Opcodes.LSHR;
import static org.objectweb.asm.Opcodes.LSTORE;
import static org.objectweb.asm.Opcodes.LSUB;
import static org.objectweb.asm.Opcodes.LUSHR;
import static org.objectweb.asm.Opcodes.LXOR;
import static org.objectweb.asm.Opcodes.MONITORENTER;
import static org.objectweb.asm.Opcodes.MONITOREXIT;
import static org.objectweb.asm.Opcodes.NEW;
import static org.objectweb.asm.Opcodes.NEWARRAY;
import static org.objectweb.asm.Opcodes.NOP;
import static org.objectweb.asm.Opcodes.POP;
import static org.objectweb.asm.Opcodes.POP2;
import static org.objectweb.asm.Opcodes.PUTFIELD;
import static org.objectweb.asm.Opcodes.PUTSTATIC;
import static org.objectweb.asm.Opcodes.RET;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.SALOAD;
import static org.objectweb.asm.Opcodes.SASTORE;
import static org.objectweb.asm.Opcodes.SIPUSH;
import static org.objectweb.asm.Opcodes.SWAP;

/**
 * Definition class used when generating a Java method for a class.
 */
@Getter
@RequiredArgsConstructor
public class MethodDef implements LispStruct {

	/**
	 * The {@link MethodVisitor} to emit code to.
	 */
	private final MethodVisitor mv;

	/**
	 * The original class definition for the method.
	 */
	private final ClassDef classDef;

	/**
	 * Emitter method to finalize method generation.
	 *
	 * @return the definition instance
	 */
	public MethodDef endMethod() {
		mv.visitEnd();
		return this;
	}

	/**
	 * Emitter method used to create a new method-level annotation.
	 *
	 * @param descriptor
	 * 		the class descriptor of the annotation class.
	 *
	 * @return the new {@link AnnotationDef}
	 */
	public AnnotationDef newAnnotation(final String descriptor) {
		final AnnotationVisitor av = mv.visitAnnotation(descriptor, true);
		return new AnnotationDef(av, this);
	}

	/**
	 * Emitter method for to adding a new try/catch block.
	 *
	 * @param start
	 * 		the beginning of the exception handler's scope (inclusive).
	 * @param end
	 * 		the end of the exception handler's scope (exclusive).
	 * @param handler
	 * 		the beginning of the exception handler's code.
	 * @param type
	 * 		the internal name of the type of exceptions handled by the handler (see {@link Type#getInternalName()}), or
	 *        {@literal null} to catch any exceptions (for "finally" blocks).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitTryCatchBlock(final Label start, final Label end, final Label handler,
	                                   final String type) {
		mv.visitTryCatchBlock(start, end, handler, type);
		return this;
	}

	/**
	 * Emitter method for Java function EMIT-LABEL and class for Lisp function for new Lisp compiler.
	 *
	 * @param label
	 * 		a {@link Label} object.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLabel(final Label label) {
		mv.visitLabel(label);
		return this;
	}

	/**
	 * Emitter method for Java opcode LINE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param line
	 * 		a line number. This number refers to the source file from which the class was compiled.
	 * @param start
	 * 		the first instruction corresponding to this line number.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLine(final int line, final Label start) {
		mv.visitLineNumber(line, start);
		return this;
	}

	/*
	NoOp
	 */

	/**
	 * Emitter method for Java opcode NOP and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitNop() {
		mv.visitInsn(NOP);
		return this;
	}

	/*
	Const
	 */

	/**
	 * Emitter method for Java opcode ACONST_NULL and Lisp function for new Lisp compiler to access Java code.
	 * <p>
	 * This emits the constant {@code null}.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAconst_null() {
		mv.visitInsn(ACONST_NULL);
		return this;
	}

	/**
	 * Emitter method for Java opcode ICONST and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param constant
	 * 		Java int constant to emit
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIconst(final int constant) {
		final int iConst = switch (constant) {
			case -1 -> ICONST_M1;
			case 0 -> ICONST_0;
			case 1 -> ICONST_1;
			case 2 -> ICONST_2;
			case 3 -> ICONST_3;
			case 4 -> ICONST_4;
			case 5 -> ICONST_5;
			default -> throw new EmitterException(
					"'iconst' called with argument " + constant + ", which is not a supported compiler constant"
			);
		};
		mv.visitInsn(iConst);
		return this;
	}

	/**
	 * Emitter method for Java opcode LCONST and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param constant
	 * 		Java long constant to emit
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLconst(final int constant) {
		final int lConst = switch (constant) {
			case 0 -> LCONST_0;
			case 1 -> LCONST_1;
			default -> throw new EmitterException(
					"'lconst' called with argument " + constant + ", which is not a supported compiler constant"
			);
		};
		mv.visitInsn(lConst);
		return this;
	}

	/**
	 * Emitter method for Java opcode FCONST and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param constant
	 * 		Java float constant to emit
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFconst(final int constant) {
		final int fConst = switch (constant) {
			case 0 -> FCONST_0;
			case 1 -> FCONST_1;
			case 2 -> FCONST_2;
			default -> throw new EmitterException(
					"'fconst' called with argument " + constant + ", which is not a supported compiler constant"
			);
		};
		mv.visitInsn(fConst);
		return this;
	}

	/**
	 * Emitter method for Java opcode DCONST and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param constant
	 * 		Java double constant to emit
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDconst(final int constant) {
		final int dConst = switch (constant) {
			case 0 -> DCONST_0;
			case 1 -> DCONST_1;
			default -> throw new EmitterException(
					"'dconst' called with argument " + constant + ", which is not a supported compiler constant"
			);
		};
		mv.visitInsn(dConst);
		return this;
	}

	/*
	Int Push
	 */

	/**
	 * Emitter method for Java opcode BIPUSH and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param operand
	 * 		the operand of the instruction to be visited. Operand value should be between Byte.MIN_VALUE and
	 * 		Byte.MAX_VALUE.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitBipush(final int operand) {
		if ((operand < Byte.MIN_VALUE) || (operand > Byte.MAX_VALUE)) {
			final String message = String.format("Invalid operand value: %s; Must be between '%s' and '%s",
			                                     operand, Byte.MIN_VALUE, Byte.MAX_VALUE);
			throw new EmitterException(message);
		}
		mv.visitIntInsn(BIPUSH, operand);
		return this;
	}

	/**
	 * Emitter method for Java opcode SIPUSH and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param operand
	 * 		the operand of the instruction to be visited. Operand value should be between Short.MIN_VALUE and
	 * 		Short.MAX_VALUE.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitSipush(final int operand) {
		if ((operand < Short.MIN_VALUE) || (operand > Short.MAX_VALUE)) {
			final String message = String.format("Invalid operand value: %s; Must be between '%s' and '%s",
			                                     operand, Short.MIN_VALUE, Short.MAX_VALUE);
			throw new EmitterException(message);
		}
		mv.visitIntInsn(SIPUSH, operand);
		return this;
	}

	/*
	LDC
	 */

	/**
	 * Emitter method for Java opcode LDC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param value
	 * 		the constant to be loaded on the stack. This parameter must be a non-null {@link Integer}, a {@link Float}, a
	 *        {@link Long}, a {@link Double}, a {@link String}, a {@link Type} of OBJECT or ARRAY sort for {@code .class}
	 * 		constants, for classes whose version is 49, a {@link Type} of METHOD sort for MethodType, a {@link Handle} for
	 * 		MethodHandle constants, for classes whose version is 51 or a {@link ConstantDynamic} for a constant dynamic for
	 * 		classes whose version is 55.
	 *
	 * @return the definition instance
	 */
	public LispStruct emitLdc(final Object value) {
		if (value == null) {
			return this;
		}
		throw new EmitterException("No constants currently supported for generic LDC");
	}

	/*
	Load
	 */

	/**
	 * Emitter method for Java opcode ILOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIload(final int varIndex) {
		mv.visitVarInsn(ILOAD, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode LLOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLload(final int varIndex) {
		mv.visitVarInsn(LLOAD, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode FLOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFload(final int varIndex) {
		mv.visitVarInsn(FLOAD, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode DLOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDload(final int varIndex) {
		mv.visitVarInsn(DLOAD, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode ALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAload(final int varIndex) {
		mv.visitVarInsn(ALOAD, varIndex);
		return this;
	}

	/*
	Array Load
	 */

	/**
	 * Emitter method for Java opcode IALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIaload() {
		mv.visitInsn(IALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode LALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLaload() {
		mv.visitInsn(LALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode FALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFaload() {
		mv.visitInsn(FALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode DALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDaload() {
		mv.visitInsn(DALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode AALOAD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAaload() {
		mv.visitInsn(AALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode BALOAD (byte array) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitBaload() {
		mv.visitInsn(BALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode CALOAD (char array) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitCaload() {
		mv.visitInsn(CALOAD);
		return this;
	}

	/**
	 * Emitter method for Java opcode SALOAD (short array) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitSaload() {
		mv.visitInsn(SALOAD);
		return this;
	}

	/*
	Store
	 */

	/**
	 * Emitter method for Java opcode ISTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIstore(final int varIndex) {
		mv.visitVarInsn(ISTORE, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode LSTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLstore(final int varIndex) {
		mv.visitVarInsn(LSTORE, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode FSTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFstore(final int varIndex) {
		mv.visitVarInsn(FSTORE, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode DSTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDstore(final int varIndex) {
		mv.visitVarInsn(DSTORE, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode ASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAstore(final int varIndex) {
		mv.visitVarInsn(ASTORE, varIndex);
		return this;
	}

	/*
	Array Store
	 */

	/**
	 * Emitter method for Java opcode IASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIastore() {
		mv.visitInsn(IASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode LASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLastore() {
		mv.visitInsn(LASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode FASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFastore() {
		mv.visitInsn(FASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode DASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDastore() {
		mv.visitInsn(DASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode AASTORE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAastore() {
		mv.visitInsn(AASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode BASTORE (byte array) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitBastore() {
		mv.visitInsn(BASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode CASTORE (char array) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitCastore() {
		mv.visitInsn(CASTORE);
		return this;
	}

	/**
	 * Emitter method for Java opcode SASTORE (short array) and Lisp function for new Lisp compiler to access Java
	 * code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitSastore() {
		mv.visitInsn(SASTORE);
		return this;
	}

	/*
	Pop/Dup/Swap
	 */

	/**
	 * Emitter method for Java opcode POP and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitPop() {
		mv.visitInsn(POP);
		return this;
	}

	/**
	 * Emitter method for Java opcode POP2 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitPop2() {
		mv.visitInsn(POP2);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup() {
		mv.visitInsn(DUP);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP_X1 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup_x1() {
		mv.visitInsn(DUP_X1);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP_X2 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup_x2() {
		mv.visitInsn(DUP_X2);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP2 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup2() {
		mv.visitInsn(DUP2);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP2_X1 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup2_x1() {
		mv.visitInsn(DUP2_X1);
		return this;
	}

	/**
	 * Emitter method for Java opcode DUP2_X2 and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDup2_x2() {
		mv.visitInsn(DUP2_X2);
		return this;
	}

	/**
	 * Emitter method for Java opcode SWAP and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitSwap() {
		mv.visitInsn(SWAP);
		return this;
	}

	/*
	Add
	 */

	/**
	 * Emitter method for Java opcode IADD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIadd() {
		mv.visitInsn(IADD);
		return this;
	}

	/**
	 * Emitter method for Java opcode LADD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLadd() {
		mv.visitInsn(LADD);
		return this;
	}

	/**
	 * Emitter method for Java opcode FADD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFadd() {
		mv.visitInsn(FADD);
		return this;
	}

	/**
	 * Emitter method for Java opcode DADD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDadd() {
		mv.visitInsn(DADD);
		return this;
	}

	/*
	Sub
	 */

	/**
	 * Emitter method for Java opcode ISUB and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIsub() {
		mv.visitInsn(ISUB);
		return this;
	}

	/**
	 * Emitter method for Java opcode LSUB and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLsub() {
		mv.visitInsn(LSUB);
		return this;
	}

	/**
	 * Emitter method for Java opcode FSUB and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFsub() {
		mv.visitInsn(FSUB);
		return this;
	}

	/**
	 * Emitter method for Java opcode DSUB and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDsub() {
		mv.visitInsn(DSUB);
		return this;
	}

	/*
	Mul
	 */

	/**
	 * Emitter method for Java opcode IMUL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitImul() {
		mv.visitInsn(IMUL);
		return this;
	}

	/**
	 * Emitter method for Java opcode LMUL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLmul() {
		mv.visitInsn(LMUL);
		return this;
	}

	/**
	 * Emitter method for Java opcode FMUL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFmul() {
		mv.visitInsn(FMUL);
		return this;
	}

	/**
	 * Emitter method for Java opcode DMUL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDmul() {
		mv.visitInsn(DMUL);
		return this;
	}

	/*
	Div
	 */

	/**
	 * Emitter method for Java opcode IDIV and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIdiv() {
		mv.visitInsn(IDIV);
		return this;
	}

	/**
	 * Emitter method for Java opcode LDIV and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLdiv() {
		mv.visitInsn(LDIV);
		return this;
	}

	/**
	 * Emitter method for Java opcode FDIV and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFdiv() {
		mv.visitInsn(FDIV);
		return this;
	}

	/**
	 * Emitter method for Java opcode DDIV and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDdiv() {
		mv.visitInsn(DDIV);
		return this;
	}

	/*
	Rem
	 */

	/**
	 * Emitter method for Java opcode IREM and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIrem() {
		mv.visitInsn(IREM);
		return this;
	}

	/**
	 * Emitter method for Java opcode LREM and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLrem() {
		mv.visitInsn(LREM);
		return this;
	}

	/**
	 * Emitter method for Java opcode FREM and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFrem() {
		mv.visitInsn(FREM);
		return this;
	}

	/**
	 * Emitter method for Java opcode DREM and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDrem() {
		mv.visitInsn(DREM);
		return this;
	}

	/*
	Neg
	 */

	/**
	 * Emitter method for Java opcode INEG and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIneg() {
		mv.visitInsn(INEG);
		return this;
	}

	/**
	 * Emitter method for Java opcode LNEG and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLneg() {
		mv.visitInsn(LNEG);
		return this;
	}

	/**
	 * Emitter method for Java opcode FNEG and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFneg() {
		mv.visitInsn(FNEG);
		return this;
	}

	/**
	 * Emitter method for Java opcode DNEG and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDneg() {
		mv.visitInsn(DNEG);
		return this;
	}

	/*
	Int/Long Shift
	 */

	/**
	 * Emitter method for Java opcode ISHL (shift left) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIshl() {
		mv.visitInsn(ISHL);
		return this;
	}

	/**
	 * Emitter method for Java opcode LSHL (shift left) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLshl() {
		mv.visitInsn(LSHL);
		return this;
	}

	/**
	 * Emitter method for Java opcode ISHR (shift right) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIshr() {
		mv.visitInsn(ISHR);
		return this;
	}

	/**
	 * Emitter method for Java opcode LSHR (shift right) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLshr() {
		mv.visitInsn(LSHR);
		return this;
	}

	/**
	 * Emitter method for Java opcode IUSHR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIushr() {
		mv.visitInsn(IUSHR);
		return this;
	}

	/**
	 * Emitter method for Java opcode LUSHR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLushr() {
		mv.visitInsn(LUSHR);
		return this;
	}

	/*
	Int/Long AND,OR
	 */

	/**
	 * Emitter method for Java opcode IAND and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIand() {
		mv.visitInsn(IAND);
		return this;
	}

	/**
	 * Emitter method for Java opcode LAND and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLand() {
		mv.visitInsn(LAND);
		return this;
	}

	/**
	 * Emitter method for Java opcode IOR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIor() {
		mv.visitInsn(IOR);
		return this;
	}

	/**
	 * Emitter method for Java opcode LOR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLor() {
		mv.visitInsn(LOR);
		return this;
	}

	/**
	 * Emitter method for Java opcode IXOR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIxor() {
		mv.visitInsn(IXOR);
		return this;
	}

	/**
	 * Emitter method for Java opcode LXOR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLxor() {
		mv.visitInsn(LXOR);
		return this;
	}

	/*
	Int Increment
	 */

	/**
	 * Emitter method for Java opcode IINC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		index of the local variable to be incremented.
	 * @param increment
	 * 		amount to increment the local variable by.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIinc(final int varIndex, final int increment) {
		mv.visitIincInsn(varIndex, increment);
		return this;
	}

	/*
	Int Conversion
	 */

	/**
	 * Emitter method for Java opcode I2L and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2l() {
		mv.visitInsn(I2L);
		return this;
	}

	/**
	 * Emitter method for Java opcode I2F and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2f() {
		mv.visitInsn(I2F);
		return this;
	}

	/**
	 * Emitter method for Java opcode I2D and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2d() {
		mv.visitInsn(I2D);
		return this;
	}

	/*
	Long Conversion
	 */

	/**
	 * Emitter method for Java opcode L2I and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitL2i() {
		mv.visitInsn(L2I);
		return this;
	}

	/**
	 * Emitter method for Java opcode L2F and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitL2f() {
		mv.visitInsn(L2F);
		return this;
	}

	/**
	 * Emitter method for Java opcode L2D and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitL2d() {
		mv.visitInsn(L2D);
		return this;
	}

	/*
	Float Conversion
	 */

	/**
	 * Emitter method for Java opcode F2I and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitF2i() {
		mv.visitInsn(F2I);
		return this;
	}

	/**
	 * Emitter method for Java opcode F2L and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitF2l() {
		mv.visitInsn(F2L);
		return this;
	}

	/**
	 * Emitter method for Java opcode F2D and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitF2d() {
		mv.visitInsn(F2D);
		return this;
	}

	/*
	Double Conversion
	 */

	/**
	 * Emitter method for Java opcode D2I and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitD2i() {
		mv.visitInsn(D2I);
		return this;
	}

	/**
	 * Emitter method for Java opcode D2L and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitD2l() {
		mv.visitInsn(D2L);
		return this;
	}

	/**
	 * Emitter method for Java opcode D2F and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitD2f() {
		mv.visitInsn(D2F);
		return this;
	}

	/*
	Int (less-precision) Conversion
	 */

	/**
	 * Emitter method for Java opcode I2B (int-to-byte) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2b() {
		mv.visitInsn(I2B);
		return this;
	}

	/**
	 * Emitter method for Java opcode I2C (int-to-char) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2c() {
		mv.visitInsn(I2C);
		return this;
	}

	/**
	 * Emitter method for Java opcode I2S (int-to-short) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitI2s() {
		mv.visitInsn(I2S);
		return this;
	}

	/*
	Long/Float/Double Compare
	 */

	/**
	 * Emitter method for Java opcode LCMP and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLcmp() {
		mv.visitInsn(LCMP);
		return this;
	}

	/**
	 * Emitter method for Java opcode FCMPL (less-than) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFcmpl() {
		mv.visitInsn(FCMPL);
		return this;
	}

	/**
	 * Emitter method for Java opcode FCMPG (greater-than) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFcmpg() {
		mv.visitInsn(FCMPG);
		return this;
	}

	/**
	 * Emitter method for Java opcode DCMPL (less-than) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDcmpl() {
		mv.visitInsn(DCMPL);
		return this;
	}

	/**
	 * Emitter method for Java opcode DCMPG (greater-than) and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDcmpg() {
		mv.visitInsn(DCMPG);
		return this;
	}

	/*
	=,!=,<,>,<=,>=
	 */

	/**
	 * Emitter method for Java opcode IFEQ and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfeq(final Label label) {
		mv.visitJumpInsn(IFEQ, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFNE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfne(final Label label) {
		mv.visitJumpInsn(IFNE, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFLT and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIflt(final Label label) {
		mv.visitJumpInsn(IFLT, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFGE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfge(final Label label) {
		mv.visitJumpInsn(IFGE, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFGT and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfgt(final Label label) {
		mv.visitJumpInsn(IFGT, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFLE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfle(final Label label) {
		mv.visitJumpInsn(IFLE, label);
		return this;
	}

	/*
	Int Compare
	 */

	/**
	 * Emitter method for Java opcode IF_ICMPEQ and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmpeq(final Label label) {
		mv.visitJumpInsn(IF_ICMPEQ, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPNE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmpne(final Label label) {
		mv.visitJumpInsn(IF_ICMPNE, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLT and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmplt(final Label label) {
		mv.visitJumpInsn(IF_ICMPLT, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmpge(final Label label) {
		mv.visitJumpInsn(IF_ICMPGE, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPGT and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmpgt(final Label label) {
		mv.visitJumpInsn(IF_ICMPGT, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ICMPLE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_icmple(final Label label) {
		mv.visitJumpInsn(IF_ICMPLE, label);
		return this;
	}

	/*
	Object Compare
	 */

	/**
	 * Emitter method for Java opcode IF_ACMPEQ and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_acmpeq(final Label label) {
		mv.visitJumpInsn(IF_ACMPEQ, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IF_ACMPNE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIf_acmpne(final Label label) {
		mv.visitJumpInsn(IF_ACMPNE, label);
		return this;
	}

	/*
	Goto,Jump,Switch
	 */

	/**
	 * Emitter method for Java opcode GOTO and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitGoto(final Label label) {
		mv.visitJumpInsn(GOTO, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode JSR and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitJsr(final Label label) {
		mv.visitJumpInsn(JSR, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode RET and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param varIndex
	 * 		the operand of the instruction to be visited. This operand is the index of a local variable.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitRet(final int varIndex) {
		mv.visitVarInsn(RET, varIndex);
		return this;
	}

	/**
	 * Emitter method for Java opcode LOOKUPSWITCH and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param dflt
	 * 		beginning of the default handler block.
	 * @param keys
	 * 		the values of the keys.
	 * @param labels
	 * 		beginnings of the handler blocks. {@code labels[i]} is the beginning of the handler block for the
	 *        {@code keys[i]} key.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLookupswitch(final Label dflt, final int[] keys, final Label[] labels) {
		mv.visitLookupSwitchInsn(dflt, keys, labels);
		return this;
	}

	/**
	 * Emitter method for Java opcode TABLESWITCH and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param min
	 * 		the minimum key value.
	 * @param max
	 * 		the maximum key value.
	 * @param dflt
	 * 		beginning of the default handler block.
	 * @param labels
	 * 		beginnings of the handler blocks. {@code labels[i]} is the beginning of the handler block for the
	 *        {@code min + i} key.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitTableswitch(final int min, final int max, final Label dflt, final Label[] labels) {
		mv.visitTableSwitchInsn(min, max, dflt, labels);
		return this;
	}

	/*
	Return
	 */

	/**
	 * Emitter method for Java opcode IRETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIreturn() {
		mv.visitInsn(IRETURN);
		return this;
	}

	/**
	 * Emitter method for Java opcode LRETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitLreturn() {
		mv.visitInsn(LRETURN);
		return this;
	}

	/**
	 * Emitter method for Java opcode FRETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitFreturn() {
		mv.visitInsn(FRETURN);
		return this;
	}

	/**
	 * Emitter method for Java opcode DRETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitDreturn() {
		mv.visitInsn(DRETURN);
		return this;
	}

	/**
	 * Emitter method for Java opcode ARETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAreturn() {
		mv.visitInsn(ARETURN);
		return this;
	}

	/**
	 * Emitter method for Java opcode RETURN and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitReturn() {
		mv.visitInsn(RETURN);
		return this;
	}

	/*
	Field Values
	 */

	/**
	 * Emitter method for Java opcode GETSTATIC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the field's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the field's name.
	 * @param descriptor
	 * 		the field's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitGetstatic(final String owner, final String name, final String descriptor) {
		mv.visitFieldInsn(GETSTATIC, owner, name, descriptor);
		return this;
	}

	/**
	 * Emitter method for Java opcode PUTSTATIC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the field's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the field's name.
	 * @param descriptor
	 * 		the field's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitPutstatic(final String owner, final String name, final String descriptor) {
		mv.visitFieldInsn(PUTSTATIC, owner, name, descriptor);
		return this;
	}

	/**
	 * Emitter method for Java opcode GETFIELD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the field's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the field's name.
	 * @param descriptor
	 * 		the field's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitGetfield(final String owner, final String name, final String descriptor) {
		mv.visitFieldInsn(GETFIELD, owner, name, descriptor);
		return this;
	}

	/**
	 * Emitter method for Java opcode PUTFIELD and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the field's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the field's name.
	 * @param descriptor
	 * 		the field's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitPutfield(final String owner, final String name, final String descriptor) {
		mv.visitFieldInsn(PUTFIELD, owner, name, descriptor);
		return this;
	}

	/*
	Method Invocation
	 */

	/**
	 * Emitter method for Java opcode INVOKEVIRTUAL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the method's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInvokevirtual(final String owner, final String name, final String descriptor) {
		mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, descriptor, false);
		return this;
	}

	/**
	 * Emitter method for Java opcode INVOKESPECIAL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the method's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInvokespecial(final String owner, final String name, final String descriptor) {
		mv.visitMethodInsn(INVOKESPECIAL, owner, name, descriptor, false);
		return this;
	}

	/**
	 * Emitter method for Java opcode INVOKESTATIC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the method's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 * @param isInterface
	 * 		if the method's owner class is an interface.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInvokestatic(final String owner, final String name, final String descriptor,
	                                  final boolean isInterface) {
		mv.visitMethodInsn(INVOKESTATIC, owner, name, descriptor, isInterface);
		return this;
	}

	/**
	 * Emitter method for Java opcode INVOKEINTERFACE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param owner
	 * 		the internal name of the method's owner class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInvokeinterface(final String owner, final String name, final String descriptor) {
		mv.visitMethodInsn(INVOKEINTERFACE, owner, name, descriptor, true);
		return this;
	}

	/**
	 * Emitter method for Java opcode INVOKEDYNAMIC and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 * @param bootstrapMethodHandle
	 * 		the bootstrap method.
	 * @param bootstrapMethodArguments
	 * 		the bootstrap method constant arguments. Each argument must be an {@link Integer}, {@link Float}, {@link Long},
	 *        {@link Double}, {@link String}, {@link Type}, {@link Handle} or {@link ConstantDynamic} value. This method is
	 * 		allowed to modify the content of the array so a caller should expect that this array may change.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInvokedynamic(final String name, final String descriptor, final Handle bootstrapMethodHandle,
	                                   final Object... bootstrapMethodArguments) {
		// TODO: do 'bootstrapMethodArguments' checks here
		mv.visitInvokeDynamicInsn(name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments);
		return this;
	}

	/*
	New Instance
	 */

	/**
	 * Emitter method for Java opcode NEW and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param type
	 * 		the operand of the instruction to be visited. This operand must be the internal name of an object or array
	 * 		class (see {@link Type#getInternalName()}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitNew(final String type) {
		mv.visitTypeInsn(NEW, type);
		return this;
	}

	/**
	 * Emitter method for Java opcode NEWARRAY and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param operand
	 * 		the operand of the instruction to be visited. Ooperand value should be one of {@link Opcodes#T_BOOLEAN},
	 *        {@link Opcodes#T_CHAR}, {@link Opcodes#T_FLOAT}, {@link Opcodes#T_DOUBLE}, {@link Opcodes#T_BYTE},
	 *        {@link Opcodes#T_SHORT}, {@link Opcodes#T_INT} or {@link Opcodes#T_LONG}.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitNewarray(final int operand) {
		switch (operand) {
			case Opcodes.T_BOOLEAN,
					Opcodes.T_CHAR,
					Opcodes.T_FLOAT,
					Opcodes.T_DOUBLE,
					Opcodes.T_BYTE,
					Opcodes.T_SHORT,
					Opcodes.T_INT,
					Opcodes.T_LONG -> {
				mv.visitIntInsn(NEWARRAY, operand);
				return this;
			}
			default -> throw new EmitterException(
					"'newarray' called with argument " + operand + ", which is not a supported value"
			);
		}
	}

	/**
	 * Emitter method for Java opcode ANEWARRAY and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param type
	 * 		the operand of the instruction to be visited. This operand must be the internal name of an object or array
	 * 		class (see {@link Type#getInternalName()}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAnewarray(final String type) {
		mv.visitTypeInsn(ANEWARRAY, type);
		return this;
	}

	/*
	Array Length
	 */

	/**
	 * Emitter method for Java opcode ARRAYLENGTH and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitArraylength() {
		mv.visitInsn(ARRAYLENGTH);
		return this;
	}

	/*
	Throw
	 */

	/**
	 * Emitter method for Java opcode ATHROW and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitAthrow() {
		mv.visitInsn(ATHROW);
		return this;
	}

	/*
	Check-cast, Instance-of
	 */

	/**
	 * Emitter method for Java opcode CHECKCAST and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param type
	 * 		the operand of the instruction to be visited. This operand must be the internal name of an object or array
	 * 		class (see {@link Type#getInternalName()}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitCheckcast(final String type) {
		mv.visitTypeInsn(CHECKCAST, type);
		return this;
	}

	/**
	 * Emitter method for Java opcode INSTANCEOF and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param type
	 * 		the operand of the instruction to be visited. This operand must be the internal name of an object or array
	 * 		class (see {@link Type#getInternalName()}).
	 *
	 * @return the definition instance
	 */
	public MethodDef emitInstanceof(final String type) {
		mv.visitTypeInsn(INSTANCEOF, type);
		return this;
	}

	/*
	Monitor
	 */

	/**
	 * Emitter method for Java opcode MONITORENTER and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitMonitorenter() {
		mv.visitInsn(MONITORENTER);
		return this;
	}

	/**
	 * Emitter method for Java opcode MONITOREXIT and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitMonitorexit() {
		mv.visitInsn(MONITOREXIT);
		return this;
	}

	/*
	Multi-Dimensional Array
	 */

	/**
	 * Emitter method for Java opcode MULTIANEWARRAY and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param descriptor
	 * 		an array type descriptor (see {@link Type}).
	 * @param numDimensions
	 * 		the number of dimensions of the array to allocate.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitMultianewarray(final String descriptor, final int numDimensions) {
		mv.visitMultiANewArrayInsn(descriptor, numDimensions);
		return this;
	}

	/*
	Null Compare
	 */

	/**
	 * Emitter method for Java opcode IFNONNULL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfnonnull(final Label label) {
		mv.visitJumpInsn(IFNONNULL, label);
		return this;
	}

	/**
	 * Emitter method for Java opcode IFNULL and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param label
	 * 		the operand of the instruction to be visited. This operand is a label that designates the instruction to which
	 * 		the jump instruction may jump.
	 *
	 * @return the definition instance
	 */
	public MethodDef emitIfnull(final Label label) {
		mv.visitJumpInsn(IFNULL, label);
		return this;
	}
}