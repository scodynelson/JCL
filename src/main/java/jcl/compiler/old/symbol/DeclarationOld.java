package jcl.compiler.old.symbol;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

public class DeclarationOld {

	public static final SymbolStruct COMPILATION_SPEED = new SymbolStruct("COMPILATION-SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct DEBUG = new SymbolStruct("DEBUG", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct DECLARATION = new SymbolStruct("DECLARATION", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct DYNAMIC_EXTENT = new SymbolStruct("DYNAMIC_EXTENT", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct FTYPE = new SymbolStruct("FTYPE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct IGNORE = new SymbolStruct("IGNORE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct IGNORABLE = new SymbolStruct("IGNORABLE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct INLINE = new SymbolStruct("INLINE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct NOTINLINE = new SymbolStruct("NOTINLINE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct OPTIMIZE = new SymbolStruct("OPTIMIZE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SAFETY = new SymbolStruct("SAFETY", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SPACE = new SymbolStruct("SPACE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SPEED = new SymbolStruct("SPEED", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SPECIAL = new SymbolStruct("SPECIAL", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct TYPE = new SymbolStruct("TYPE", GlobalPackageStruct.COMMON_LISP);

	// Here are the declarations that are NOT Common Lisp but used to pass information
	// around for the compiler or other internal systems.
	public static final SymbolStruct BASE_LISP_FNS = new SymbolStruct("CoreFunctionsLsp", GlobalPackageStruct.SYSTEM);

	public static final SymbolStruct DOCUMENTATION = new SymbolStruct("DOCUMENTATION", GlobalPackageStruct.SYSTEM);

	public static final SymbolStruct JAVA_CLASS_NAME = new SymbolStruct("%JAVA-CLASS-NAME", GlobalPackageStruct.SYSTEM);

	public static final SymbolStruct LISP_NAME = new SymbolStruct("%LISP-NAME", GlobalPackageStruct.SYSTEM);

	public static final SymbolStruct NO_GENERATE_ANALYZER = new SymbolStruct("%NO-GENERATE-ANALYZER", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct PARSED_LAMBDA_LIST = new SymbolStruct("%PARSED-LAMBDA-LIST", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct SOURCE_FILE = new SymbolStruct("%SOURCE_FILE", GlobalPackageStruct.COMPILER);
}
