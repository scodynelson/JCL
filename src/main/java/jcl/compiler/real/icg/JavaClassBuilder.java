/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.ClassWriter;

public class JavaClassBuilder {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

	private final String fileName;

	private final String className;

	public JavaClassBuilder(final String fileName, final String className) {
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
}
