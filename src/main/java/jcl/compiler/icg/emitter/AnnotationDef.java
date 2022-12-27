package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Type;

/**
 * Definition class used when generating a Java annotation for a class, field, or method.
 */
@Getter
public class AnnotationDef implements LispStruct {

	/**
	 * The {@link AnnotationVisitor} to emit code to.
	 */
	private final AnnotationVisitor av;

	/**
	 * The original class definition for the annotation.
	 */
	private final ClassDef classDef;

	/**
	 * The original field definition for the annotation.
	 */
	private final FieldDef fieldDef;

	/**
	 * The original method definition for the annotation.
	 */
	private final MethodDef methodDef;

	/**
	 * Create a new instance for a class-level annotation.
	 *
	 * @param av
	 * 		the {@link AnnotationVisitor} to emit code to.
	 * @param classDef
	 * 		the original class definition for the annotation.
	 */
	public AnnotationDef(final AnnotationVisitor av, final ClassDef classDef) {
		this.av = av;
		this.classDef = classDef;
		fieldDef = null;
		methodDef = null;
	}

	/**
	 * Create a new instance for a field-level annotation.
	 *
	 * @param av
	 * 		the {@link AnnotationVisitor} to emit code to.
	 * @param fieldDef
	 * 		the original field definition for the annotation.
	 */
	public AnnotationDef(final AnnotationVisitor av, final FieldDef fieldDef) {
		this.av = av;
		classDef = null;
		this.fieldDef = fieldDef;
		methodDef = null;
	}

	/**
	 * Create a new instance for a method-level annotation.
	 *
	 * @param av
	 * 		the {@link AnnotationVisitor} to emit code to.
	 * @param methodDef
	 * 		the original method definition for the annotation.
	 */
	public AnnotationDef(final AnnotationVisitor av, final MethodDef methodDef) {
		this.av = av;
		classDef = null;
		fieldDef = null;
		this.methodDef = methodDef;
	}

	/**
	 * Emitter method to finalize annotation generation.
	 *
	 * @return the definition instance
	 */
	public AnnotationDef endAnnotation() {
		av.visitEnd();
		return this;
	}

	/**
	 * Emitter method for Java function EMIT-ANNOTATION-FIELD and class for Lisp function for new Lisp compiler.
	 *
	 * @param name
	 * 		the value name.
	 * @param value
	 * 		the actual value, whose type must be {@link Byte}, {@link Boolean}, {@link Character}, {@link Short},
	 *        {@link Integer} , {@link Long}, {@link Float}, {@link Double}, {@link String} or {@link Type} of
	 *        {@link Type#OBJECT} or {@link Type#ARRAY} sort. This value can also be an array of byte, boolean, short, char,
	 * 		int, long, float or double values (this is equivalent to using {@link AnnotationVisitor#visitArray(String)} and
	 * 		visiting each array element in turn, but is more convenient).
	 *
	 * @return the definition instance
	 */
	public AnnotationDef emitAnnotationField(final String name, final Object value) {
		// TODO: do 'value' checks here
		av.visit(name, value.toString());
		return this;
	}
}