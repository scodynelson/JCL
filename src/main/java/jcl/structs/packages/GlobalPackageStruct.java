package jcl.structs.packages;

import jcl.structs.KeywordPackageStruct;
import jcl.structs.PackageStruct;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code GlobalPackageStruct} is the global location for system defined packages.
 */
public interface GlobalPackageStruct {

	Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP", Collections.singletonList("CL"));
	PackageStruct SYSTEM = new PackageStruct("SYSTEM");
	PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER", Collections.singletonList("CL-USER"));
	PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;
}
