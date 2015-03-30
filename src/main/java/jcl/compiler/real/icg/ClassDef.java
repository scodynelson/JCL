/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import java.util.Stack;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;

public class ClassDef {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

	private final String fileName;

	private final String className;

	private final Stack<Integer> closureStoreStack;

	private MethodVisitor methodVisitor;

	private FieldVisitor fieldVisitor;

	private AnnotationVisitor annotationVisitor;

	private int nextAvailableStore;

	public ClassDef(final String fileName, final String className, final Stack<Integer> closureStoreStack) {
		this.fileName = fileName;
		this.className = className;
		this.closureStoreStack = closureStoreStack;
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

	public String getFileName() {
		return fileName;
	}

	public String getClassName() {
		return className;
	}

	public Stack<Integer> getClosureStoreStack() {
		return closureStoreStack;
	}

	public int getCurrentStore() {
		return nextAvailableStore;
	}

	public int getNextAvailableStore() {
		int realNextAvailableStore = nextAvailableStore;
		nextAvailableStore++;
		while (closureStoreStack.contains(nextAvailableStore)) {
			realNextAvailableStore = nextAvailableStore;
			nextAvailableStore++;
		}
		return realNextAvailableStore;
	}

	public void resetStores() {
		nextAvailableStore = 0;
	}
}
