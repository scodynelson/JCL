package jcl.packages;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
public interface GlobalPackageStruct {

	Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP", Collections.singletonList("CL"));
	PackageStruct SYSTEM = new PackageStruct("SYSTEM");
	PackageStruct COMPILER = new PackageStruct("COMPILER");
	PackageStruct BACKQUOTE = new PackageStruct("BACKQUOTE");
	PackageStruct JCL_TYPE = new PackageStruct("JCL-TYPE");
	PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER", Collections.singletonList("CL-USER"), Collections.singleton(COMMON_LISP));
	PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;
}
