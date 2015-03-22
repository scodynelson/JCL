package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;

public final class Declaration extends SymbolStruct<Declaration> {

	// TODO: These are NOT declared in the "COMMON_LISP" package. They probably exist in the "SYSTEM" package instead. Have to work this in...
	public static final Declaration IGNORE = new Declaration("IGNORE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration IGNORABLE = new Declaration("IGNORABLE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration DYNAMIC_EXTENT = new Declaration("DYNAMIC-EXTENT", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration TYPE = new Declaration("TYPE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration INLINE = new Declaration("INLINE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration NOTINLINE = new Declaration("NOTINLINE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration FTYPE = new Declaration("FTYPE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration DECLARATION = new Declaration("DECLARATION", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration OPTIMIZE = new Declaration("OPTIMIZE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration SPECIAL = new Declaration("SPECIAL", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration COMPILATION_SPEED = new Declaration("COMPILATION-SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration DEBUG = new Declaration("DEBUG", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration SAFETY = new Declaration("SAFETY", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration SPACE = new Declaration("SPACE", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration SPEED = new Declaration("SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final Declaration DOCUMENTATION = new Declaration("%DOCUMENTATION", GlobalPackageStruct.SYSTEM);

	public static final Declaration JAVA_CLASS_NAME = new Declaration("%JAVA-CLASS-NAME", GlobalPackageStruct.SYSTEM);

	public static final Declaration LISP_NAME = new Declaration("%LISP-NAME", GlobalPackageStruct.SYSTEM);

	private static final long serialVersionUID = -7171797715492689333L;

	private Declaration(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		setValue(this);
	}
}
