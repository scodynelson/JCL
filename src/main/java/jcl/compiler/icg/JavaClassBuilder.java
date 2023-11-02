/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import jcl.compiler.classloaders.CompilerClassLoader;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.ClassWriter;

@Getter
@RequiredArgsConstructor
public class JavaClassBuilder {

	private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
		@Override
		protected ClassLoader getClassLoader() {
			return CompilerClassLoader.INSTANCE;
		}
	};
	private final String className;
	private final String fileName;
	private final Map<String, Set<Integer>> nonPackageSymbolFields = new HashMap<>();
}
