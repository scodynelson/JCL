/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import java.util.Stack;

import org.objectweb.asm.ClassWriter;

public class ClassDef {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

	private final String fileName;

	private final String className;

	private final Stack<Integer> closureStoreStack = new Stack<>();

	public ClassDef(final String fileName, final String className) {
		this.fileName = fileName;
		this.className = className;
	}

	public ClassWriter getClassWriter() {
		return classWriter;
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
}
