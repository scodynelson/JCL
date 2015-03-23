/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;

public class ClassDef {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

	private final String name;

	private final String fileName;

	private MethodVisitor methodVisitor;

	private FieldVisitor fieldVisitor;

	private AnnotationVisitor annotationVisitor;

	private int nextAvailableStore;

	public ClassDef(final String name, final String fileName) {
		this.name = name;
		this.fileName = fileName;
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

	public String getFileName() {
		return fileName;
	}

	public int getCurrentStore() {
		return nextAvailableStore;
	}

	public int getNextAvailableStore() {
		return nextAvailableStore++;
	}
}
