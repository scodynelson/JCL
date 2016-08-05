/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.KeywordPackageStructImpl;
import jcl.lang.PackageStructImpl;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
@SuppressWarnings("all")
public final class GlobalPackageStruct {

	public static final Map<String, PackageStructImpl> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStructImpl EXTENSIONS = PackageStructImpl.valueOf("EXTENSIONS");

	public static final PackageStructImpl COMPILER = PackageStructImpl.valueOf("COMPILER");

	public static final PackageStructImpl BACKQUOTE = PackageStructImpl.valueOf("BACKQUOTE");

	public static final PackageStructImpl COMMON_LISP = PackageStructImpl.valueOf("COMMON-LISP", Collections.singletonList("CL"), COMPILER, BACKQUOTE);

	public static final PackageStructImpl SYSTEM = PackageStructImpl.valueOf("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE, COMMON_LISP);

	public static final PackageStructImpl JCL_TYPE = PackageStructImpl.valueOf("JCL-TYPE");

	public static final PackageStructImpl COMMON_LISP_USER = PackageStructImpl.valueOf("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS);

	public static final PackageStructImpl KEYWORD = KeywordPackageStructImpl.INSTANCE;

	private GlobalPackageStruct() {
	}
}
