package jcl.compiler.old;

import jcl.system.JCL;
import jcl.types.NIL;

import java.net.URL;

public class CompilerClassLoader extends ClassLoader {

	public static final CompilerClassLoader Loader = new CompilerClassLoader(ClassLoader.getSystemClassLoader());

	/**
	 * Creates a new instance of CompilerClassLoader
	 */
	public CompilerClassLoader() {
		super(JCL.CURRENT_CLASSLOADER);
		JCL.CURRENT_CLASSLOADER = this;
	}

	private CompilerClassLoader(ClassLoader classLoader) {
		super(classLoader);
		JCL.CURRENT_CLASSLOADER = this;
	}

	public Class<?> loadClass(byte[] bos, String name) {
		Class<?> c;
		c = defineClass(name.replace('/', '.'), bos, 0, bos.length);
		resolveClass(c);
		return c;
	}

	/**
	 * Defines a package with the given name,
	 * specTitle, specVersion, specVendor, implTitle, implVersion,
	 * implVendor, and sealBase.
	 *
	 * @param arg1
	 * @param arg2
	 * @param arg3
	 * @param arg4
	 * @param arg5
	 * @param arg6
	 * @param arg7
	 * @param arg8
	 * @return Defined Package
	 */
	public Package defineJavaPackage(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8) {
		URL sealBase = null;
		String[] holder = new String[7];
		for (int i = 0; i < holder.length; i++) {
			holder[i] = null;
		}
		if (arg1 != null) {
			holder[0] = arg1.toString();
		}
		if (arg2 != NIL.INSTANCE) {
			holder[1] = arg2.toString();
		}
		if (arg3 != NIL.INSTANCE) {
			holder[2] = arg2.toString();
		}
		if (arg4 != NIL.INSTANCE) {
			holder[3] = arg2.toString();
		}
		if (arg5 != NIL.INSTANCE) {
			holder[4] = arg2.toString();
		}
		if (arg6 != NIL.INSTANCE) {
			holder[5] = arg2.toString();
		}
		if (arg7 != NIL.INSTANCE) {
			holder[6] = arg2.toString();
		}
		if (arg8 != NIL.INSTANCE && arg8 instanceof URL) {
			sealBase = (URL) arg8;
		}
		return this.definePackage(holder[0], holder[1], holder[2], holder[3], holder[4], holder[5], holder[6], sealBase);
	}

	/**
	 * Returns a Package that has been defined by
	 * this class loader or any of its ancestors.
	 *
	 * @param name - The package name
	 * @return The Package corresponding to the given name, or null if not found
	 */
	public Package getJavaPackage(String name) {
		return this.getPackage(name);
	}
}
