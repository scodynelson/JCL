/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.classloaders;

public class CompilerClassLoader extends ClassLoader {

	public static final CompilerClassLoader INSTANCE = new CompilerClassLoader(ClassLoader.getSystemClassLoader());

	private CompilerClassLoader(final ClassLoader classLoader) {
		super(classLoader);
	}

	public synchronized Class<?> loadClass(final String name, final byte[] byteArray) {
		final Class<?> clazz = defineClass(name.replace('/', '.'), byteArray, 0, byteArray.length);
		resolveClass(clazz);
		return clazz;
	}
}
