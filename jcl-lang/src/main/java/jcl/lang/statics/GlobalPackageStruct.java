/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.KeywordPackageStructImpl;
import jcl.lang.PackageStruct;
import jcl.lang.PackageStructImpl;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
@SuppressWarnings("all")
public final class GlobalPackageStruct {

	public static final Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStruct EXTENSIONS = PackageStructImpl.valueOf("EXTENSIONS");

	public static final PackageStruct COMPILER = PackageStructImpl.valueOf("COMPILER");

	public static final PackageStruct BACKQUOTE = PackageStructImpl.valueOf("BACKQUOTE");

	public static final PackageStruct COMMON_LISP = PackageStructImpl.valueOf("COMMON-LISP", Collections.singletonList("CL"), COMPILER, BACKQUOTE);

	public static final PackageStruct SYSTEM = PackageStructImpl.valueOf("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE, COMMON_LISP);

	public static final PackageStruct JCL_TYPE = PackageStructImpl.valueOf("JCL-TYPE");

	public static final PackageStruct COMMON_LISP_USER = PackageStructImpl.valueOf("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS);

	public static final PackageStruct KEYWORD = KeywordPackageStructImpl.INSTANCE;

	private GlobalPackageStruct() {
	}
}
