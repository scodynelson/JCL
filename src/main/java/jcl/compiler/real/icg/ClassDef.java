/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;

public class ClassDef {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);

	private final String name;

	private MethodVisitor methodVisitor;

	private FieldVisitor fieldVisitor;

	private AnnotationVisitor annotationVisitor;

	public ClassDef(final String name) {
		this.name = name;
	}

	public ClassWriter getClassWriter() {
		return classWriter;
	}

	public MethodVisitor getMethodVisitor() {
		return methodVisitor;
	}

	public void setMethodVisitor(final MethodVisitor methodVisitor) {
		this.methodVisitor = methodVisitor;
	}

	public FieldVisitor getFieldVisitor() {
		return fieldVisitor;
	}

	public void setFieldVisitor(final FieldVisitor fieldVisitor) {
		this.fieldVisitor = fieldVisitor;
	}

	public AnnotationVisitor getAnnotationVisitor() {
		return annotationVisitor;
	}

	public void setAnnotationVisitor(final AnnotationVisitor annotationVisitor) {
		this.annotationVisitor = annotationVisitor;
	}

	public String getName() {
		return name;
	}
}
