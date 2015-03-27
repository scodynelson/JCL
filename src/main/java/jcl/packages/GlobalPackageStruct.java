/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

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
	PackageStruct EXTENSIONS = new PackageStruct("EXTENSIONS");
	PackageStruct COMPILER = new PackageStruct("COMPILER");
	PackageStruct BACKQUOTE = new PackageStruct("BACKQUOTE");
	PackageStruct SYSTEM = new PackageStruct("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE);
	PackageStruct JCL_TYPE = new PackageStruct("JCL-TYPE");
	PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS, SYSTEM, JCL_TYPE);
	PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;
}
