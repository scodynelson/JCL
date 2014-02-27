package jcl.structs.packages;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code GlobalPackageStruct} is the global location for system defined packages.
 */
public interface GlobalPackageStruct {

	Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP");
	PackageStruct SYSTEM = new PackageStruct("SYSTEM");
	PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER");
	PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;
}
