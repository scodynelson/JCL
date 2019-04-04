/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.PackageStruct;
import jcl.lang.internal.KeywordPackageStructImpl;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
@SuppressWarnings("all")
public final class GlobalPackageStruct {

	public static final Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	public static final PackageStruct COMPILER = PackageStruct.toLispPackage("COMPILER");

	public static final PackageStruct BACKQUOTE = PackageStruct.toLispPackage("BACKQUOTE");

	public static final PackageStruct COMMON_LISP = PackageStruct.toLispPackage("COMMON-LISP", Collections.singletonList("CL"), COMPILER, BACKQUOTE);

	public static final PackageStruct SYSTEM = PackageStruct.toLispPackage("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE, COMMON_LISP);

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStruct EXTENSIONS = PackageStruct.toLispPackage("EXTENSIONS", Collections.singletonList("EXT"), SYSTEM, COMPILER, BACKQUOTE, COMMON_LISP);

	public static final PackageStruct JCL_TYPE = PackageStruct.toLispPackage("JCL-TYPE");

	public static final PackageStruct COMMON_LISP_USER = PackageStruct.toLispPackage("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS);

	public static final PackageStruct KEYWORD = KeywordPackageStructImpl.INSTANCE;

	private GlobalPackageStruct() {
	}
}
