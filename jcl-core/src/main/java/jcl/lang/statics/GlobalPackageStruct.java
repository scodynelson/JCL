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

	public static final PackageStruct COMMON_LISP = PackageStruct.toLispPackage("COMMON-LISP", Collections.singletonList("CL"));

	public static final PackageStruct SYSTEM = PackageStruct.toLispPackage("SYSTEM", Collections.emptyList());

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStruct EXTENSIONS = PackageStruct.toLispPackage("EXTENSIONS", Collections.singletonList("EXT"));

	public static final PackageStruct COMMON_LISP_USER = PackageStruct.toLispPackage("COMMON-LISP-USER", Collections.singletonList("CL-USER"));

	public static final PackageStruct KEYWORD = KeywordPackageStructImpl.INSTANCE;

	static {
		COMMON_LISP.usePackage(COMPILER);
		COMMON_LISP.usePackage(BACKQUOTE);

		SYSTEM.usePackage(COMPILER);
		SYSTEM.usePackage(BACKQUOTE);
		SYSTEM.usePackage(COMMON_LISP);

		EXTENSIONS.usePackage(SYSTEM);
		EXTENSIONS.usePackage(COMPILER);
		EXTENSIONS.usePackage(BACKQUOTE);
		EXTENSIONS.usePackage(COMMON_LISP);

		COMMON_LISP_USER.usePackage(COMMON_LISP);
		COMMON_LISP_USER.usePackage(EXTENSIONS);
	}

	private GlobalPackageStruct() {
	}
}
