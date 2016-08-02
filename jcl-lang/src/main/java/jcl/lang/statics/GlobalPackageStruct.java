/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.KeywordPackageStruct;
import jcl.lang.PackageStruct;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
@SuppressWarnings("all")
public final class GlobalPackageStruct {

	public static final Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStruct EXTENSIONS = PackageStruct.valueOf("EXTENSIONS");

	public static final PackageStruct COMPILER = PackageStruct.valueOf("COMPILER");

	public static final PackageStruct BACKQUOTE = PackageStruct.valueOf("BACKQUOTE");

	public static final PackageStruct COMMON_LISP = PackageStruct.valueOf("COMMON-LISP", Collections.singletonList("CL"), COMPILER, BACKQUOTE);

	public static final PackageStruct SYSTEM = PackageStruct.valueOf("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE, COMMON_LISP);

	public static final PackageStruct JCL_TYPE = PackageStruct.valueOf("JCL-TYPE");

	public static final PackageStruct COMMON_LISP_USER = PackageStruct.valueOf("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS);

	public static final PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;

	private GlobalPackageStruct() {
	}
}
