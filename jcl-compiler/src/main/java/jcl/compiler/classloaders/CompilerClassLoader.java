/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.classloaders;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class CompilerClassLoader extends ClassLoader {

	public static final CompilerClassLoader INSTANCE = new CompilerClassLoader(ClassLoader.getSystemClassLoader());

	private final Map<String, byte[]> loadedClasses = new HashMap<>();

	private CompilerClassLoader(final ClassLoader classLoader) {
		super(classLoader);
	}

	public synchronized Class<?> loadClass(final String name, final byte[] byteArray) {
		final Class<?> clazz = defineClass(name, byteArray, 0, byteArray.length);
		resolveClass(clazz);
		loadedClasses.put(name.replace('.', '/') + ".class", byteArray);
		return clazz;
	}

	@Override
	public InputStream getResourceAsStream(final String name) {
		if (loadedClasses.containsKey(name)) {
			return new ByteArrayInputStream(loadedClasses.get(name));
		}
		return super.getResourceAsStream(name);
	}
}
