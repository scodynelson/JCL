package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.FieldVisitor;

/**
 * Definition class used when generating a Java field for a class.
 */
@Getter
@RequiredArgsConstructor
public class FieldDef implements LispStruct {

	/**
	 * The {@link FieldVisitor} to emit code to.
	 */
	private final FieldVisitor fv;

	/**
	 * The original class definition for the field.
	 */
	private final ClassDef classDef;

	/**
	 * Emitter method to finalize field generation.
	 *
	 * @return the definition instance
	 */
	public FieldDef endField() {
		fv.visitEnd();
		return this;
	}

	/**
	 * Emitter method used to create a new field-level annotation.
	 *
	 * @param descriptor
	 * 		the class descriptor of the annotation class.
	 *
	 * @return the new {@link AnnotationDef}
	 */
	public AnnotationDef newAnnotation(final String descriptor) {
		final AnnotationVisitor av = fv.visitAnnotation(descriptor, true);
		return new AnnotationDef(av, this);
	}
}