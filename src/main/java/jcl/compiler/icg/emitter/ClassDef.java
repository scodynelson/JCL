package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * Definition class used when generating a Java class.
 */
@Getter
@RequiredArgsConstructor
public class ClassDef implements LispStruct {

	/**
	 * The {@link ClassVisitor} to emit code to.
	 */
	private final ClassVisitor cv;

	/**
	 * Emitter method for Java opcode SOURCE and Lisp function for new Lisp compiler to access Java code.
	 *
	 * @param source
	 * 		the name of the source file from which the class was compiled. May be {@literal null}.
	 *
	 * @return the definition instance
	 */
	public ClassDef emitSource(final String source) {
		cv.visitSource(source, null);
		return this;
	}

	/**
	 * Emitter method to finalize class generation.
	 *
	 * @return the definition instance
	 */
	public ClassDef endClass() {
		cv.visitEnd();
		return this;
	}

	/**
	 * Emitter method used to create a new class-level annotation.
	 *
	 * @param descriptor
	 * 		the class descriptor of the annotation class.
	 *
	 * @return the new {@link AnnotationDef}
	 */
	public AnnotationDef newAnnotation(final String descriptor) {
		final AnnotationVisitor av = cv.visitAnnotation(descriptor, true);
		return new AnnotationDef(av, this);
	}

	/**
	 * Emitter method for Java function NEW-CLASS and class for Lisp function for new Lisp compiler.
	 *
	 * @param access
	 * 		the class's access flags (see {@link Opcodes}). This parameter also indicates if the class is deprecated
	 *        {@link Opcodes#ACC_DEPRECATED} or a record {@link Opcodes#ACC_RECORD}.
	 * @param name
	 * 		the internal name of the class (see {@link Type#getInternalName()}).
	 * @param signature
	 * 		the signature of this class. May be {@literal null} if the class is not a generic one, and does not extend or
	 * 		implement generic classes or interfaces.
	 * @param superName
	 * 		the internal of name of the super class (see {@link Type#getInternalName()}). For interfaces, the super class
	 * 		is {@link Object}. May be {@literal null}, but only for the {@link Object} class.
	 * @param interfaces
	 * 		the internal names of the class's interfaces (see {@link Type#getInternalName()}). May be {@literal null}.
	 *
	 * @return the definition instance
	 */
	public ClassDef visit(final int access, final String name, final String signature, final String superName,
	                      final String[] interfaces) {
		cv.visit(Opcodes.V19, access, name, signature, superName, interfaces);
		return this;
	}

	/**
	 * Emitter method for Java function ADD-INNER-CLASS-REF and class for Lisp function for new Lisp compiler.
	 *
	 * @param name
	 * 		the internal name of C (see {@link Type#getInternalName()}).
	 * @param outerName
	 * 		the internal name of the class or interface C is a member of (see {@link Type#getInternalName()}). Must be
	 *        {@literal null} if C is not the member of a class or interface (e.g. for local or anonymous classes).
	 * @param innerName
	 * 		the (simple) name of C. Must be {@literal null} for anonymous inner classes.
	 * @param access
	 * 		the access flags of C originally declared in the source code from which this class was compiled.
	 *
	 * @return the definition instance
	 */
	public ClassDef addInnerClassReference(final String name, final String outerName, final String innerName,
	                                       final int access) {
		cv.visitInnerClass(name, outerName, innerName, access);
		return this;
	}

	/**
	 * Emitter method for Java function ADD-OUTER-CLASS-REF and class for Lisp function for new Lisp compiler.
	 *
	 * @param owner
	 * 		internal name of the enclosing class of the class (see {@link Type#getInternalName()}).
	 * @param name
	 * 		the name of the method that contains the class, or {@literal null} if the class is not enclosed in a method or
	 * 		constructor of its enclosing class (e.g. if it is enclosed in an instance initializer, static initializer,
	 * 		instance variable initializer, or class variable initializer).
	 * @param descriptor
	 * 		the descriptor of the method that contains the class, or {@literal null} if the class is not enclosed in a
	 * 		method or constructor of its enclosing class (e.g. if it is enclosed in an instance initializer, static
	 * 		initializer, instance variable initializer, or class variable initializer).
	 *
	 * @return the definition instance
	 */
	public ClassDef addOuterClassReference(final String owner, final String name, final String descriptor) {
		cv.visitOuterClass(owner, name, descriptor);
		return this;
	}

	/**
	 * Emitter method for Java function ADD-FIELD and class for Lisp function for new Lisp compiler.
	 *
	 * @param access
	 * 		the field's access flags (see {@link Opcodes}). This parameter also indicates if the field is synthetic and/or
	 * 		deprecated.
	 * @param name
	 * 		the field's name.
	 * @param descriptor
	 * 		the field's descriptor (see {@link Type}).
	 * @param signature
	 * 		the field's signature. May be {@literal null} if the field's type does not use generic types.
	 * @param value
	 * 		the field's initial value. This parameter, which may be {@literal null} if the field does not have an initial
	 * 		value, must be an {@link Integer}, a {@link Float}, a {@link Long}, a {@link Double} or a {@link String} (for
	 *        {@code int}, {@code float}, {@code long} or {@code String} fields respectively). <i>This parameter is only used
	 * 		for static fields</i>. Its value is ignored for non-static fields, which must be initialized through bytecode
	 * 		instructions in constructors or methods.
	 *
	 * @return addField Object addField
	 */
	public FieldDef newField(final int access, final String name, final String descriptor, final String signature,
	                         final Object value) {
		final FieldVisitor fv = cv.visitField(access, name, descriptor, signature, value);
		return new FieldDef(fv, this);
	}

	/**
	 * Emitter method for Java function NEW-METHOD and class for Lisp function for new Lisp compiler.
	 *
	 * @param access
	 * 		the method's access flags (see {@link Opcodes}). This parameter also indicates if the method is synthetic
	 * 		and/or deprecated.
	 * @param name
	 * 		the method's name.
	 * @param descriptor
	 * 		the method's descriptor (see {@link Type}).
	 * @param signature
	 * 		the method's signature. May be {@literal null} if the method parameters, return type and exceptions do not use
	 * 		generic types.
	 * @param exceptions
	 * 		the internal names of the method's exception classes (see {@link Type#getInternalName()}). May be
	 *        {@literal null}.
	 *
	 * @return newMethod Object newMethod
	 */
	public MethodDef newMethod(final int access, final String name, final String descriptor, final String signature,
	                           final String[] exceptions) {
		final MethodVisitor mv = cv.visitMethod(access, name, descriptor, signature, exceptions);
		mv.visitCode();
		return new MethodDef(mv, this);
	}
}