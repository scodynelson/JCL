/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.objectweb.asm.ClassWriter;

public class JavaClassBuilder {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

	private final String className;

	private final String fileName;

	private final Map<String, Set<Integer>> nonPackageSymbolFields = new HashMap<>();

	public JavaClassBuilder(final String className, final String fileName) {
		this.className = className;
		this.fileName = fileName;
	}

	public ClassWriter getClassWriter() {
		return classWriter;
	}

	public String getClassName() {
		return className;
	}

	public String getFileName() {
		return fileName;
	}

	public Map<String, Set<Integer>> getNonPackageSymbolFields() {
		return nonPackageSymbolFields;
	}
}
