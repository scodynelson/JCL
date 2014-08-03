package jcl.structs.symbols;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;

public final class Declaration extends SymbolStruct<Declaration> {

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

	public static final Declaration NO_GENERATE_ANALYZER = new Declaration("%NO-GENERATE-ANALYZER", GlobalPackageStruct.COMPILER);
	public static final Declaration PARSED_LAMBDA_LIST = new Declaration("%PARSED-LAMBDA-LIST", GlobalPackageStruct.COMPILER);
	public static final Declaration SOURCE_FILE = new Declaration("%SOURCE_FILE", GlobalPackageStruct.COMPILER);

	private Declaration(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}
}