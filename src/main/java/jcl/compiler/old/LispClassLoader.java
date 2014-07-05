package jcl.compiler.old;

import jcl.system.JCL;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.Hashtable;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class LispClassLoader extends ClassLoader {

	private Hashtable classes = new Hashtable();
	private JarFile jarFile;

	public LispClassLoader(JarFile jarFile) {
		super(JCL.CURRENT_CLASSLOADER);
		JCL.CURRENT_CLASSLOADER = this;
		this.jarFile = jarFile;
	}

//    public LispClassLoader(String jarFileName) {
//        super(lisp.common.CLforJava.CURRENT_CLASSLOADER);
//        try {
//            jarFile = new JarFile(new File(jarFileName));
//        } catch (Exception ex) {
//            System.out.println("Unable to create class loader from jar file: " + jarFileName);
//        }
//        lisp.common.CLforJava.CURRENT_CLASSLOADER = this;
//    }

	//    public void changeJarFile(JarFile jarFile) {
//        f = jarFile;
//    }
//
	@Override
	public Class loadClass(String className) throws ClassNotFoundException {
//        System.out.println("loadClass name: " + className);
		return (loadClass(className, true));
	}

	@SuppressWarnings("unchecked")
	@Override
	public synchronized Class loadClass(String className, boolean resolve) throws ClassNotFoundException {
		if (classes.containsKey(className)) {
			return (Class) classes.get(className);
		}
		BufferedInputStream bis = null;
		byte[] res = null;

		JarEntry jarEntry = jarFile.getJarEntry(className.replace('.', '/') + ".class");
		Class nextClass = null;
		// look for the next class in the class tree
		if (jarEntry == null) {
			try {
				nextClass = super.findClass(className);
			} catch (ClassNotFoundException ex) {
				try {
					nextClass = super.findSystemClass(className);
				} catch (ClassNotFoundException ex1) {
					System.out.println("Unable to locate the Class: " + className);
				}
			}
			return nextClass;
		}

		res = new byte[(int) jarEntry.getSize()];
		try {
			bis = new BufferedInputStream(jarFile.getInputStream(jarEntry));
			bis.read(res, 0, res.length);
		} catch (Exception ex) {
			System.out.println("Runtime exception reading the class " + jarEntry + " from JAR file " + jarFile + '\n' + ex);
		} finally {
			if (bis != null) {
				try {
					bis.close();
				} catch (IOException ioex) {
					System.out.println("** IOException: " + ioex);
				}
			}
		}
		Class clazz = defineClass(className, res, 0, res.length);
		if (clazz == null) {
			throw new ClassFormatError();
		}

		if (resolve) {
			resolveClass(clazz);
		}
		classes.put(className, clazz);
		return (clazz);
	}
}
