package jcl.lang;

import jcl.lang.statics.GlobalPackageStruct;

public final class DeclarationStruct extends SymbolStruct {

	// TODO: These are NOT declared in the "COMMON_LISP" package. They probably exist in the "SYSTEM" package instead. Have to work this in...
	public static final DeclarationStruct IGNORE = new DeclarationStruct("IGNORE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct IGNORABLE = new DeclarationStruct("IGNORABLE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct DYNAMIC_EXTENT = new DeclarationStruct("DYNAMIC-EXTENT", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct TYPE = new DeclarationStruct("TYPE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct INLINE = new DeclarationStruct("INLINE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct NOTINLINE = new DeclarationStruct("NOTINLINE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct FTYPE = new DeclarationStruct("FTYPE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct DECLARATION = new DeclarationStruct("DECLARATION", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct OPTIMIZE = new DeclarationStruct("OPTIMIZE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct SPECIAL = new DeclarationStruct("SPECIAL", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct COMPILATION_SPEED = new DeclarationStruct("COMPILATION-SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct DEBUG = new DeclarationStruct("DEBUG", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct SAFETY = new DeclarationStruct("SAFETY", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct SPACE = new DeclarationStruct("SPACE", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct SPEED = new DeclarationStruct("SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final DeclarationStruct DOCUMENTATION = new DeclarationStruct("%DOCUMENTATION", GlobalPackageStruct.SYSTEM);

	public static final DeclarationStruct JAVA_CLASS_NAME = new DeclarationStruct("%JAVA-CLASS-NAME", GlobalPackageStruct.SYSTEM);

	public static final DeclarationStruct LISP_NAME = new DeclarationStruct("%LISP-NAME", GlobalPackageStruct.SYSTEM);

	private DeclarationStruct(final String name, final PackageStruct symbolPackage) {
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
