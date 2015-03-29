/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.classloaders;

public class CompilerClassLoader extends ClassLoader {

	public static final CompilerClassLoader INSTANCE = new CompilerClassLoader(ClassLoader.getSystemClassLoader());

	private CompilerClassLoader(final ClassLoader classLoader) {
		super(classLoader);
	}

	public synchronized Class<?> loadClass(final byte[] bos, final String name) {
		final Class<?> clazz = defineClass(name.replace('/', '.'), bos, 0, bos.length);
		resolveClass(clazz);
		return clazz;
	}
}
