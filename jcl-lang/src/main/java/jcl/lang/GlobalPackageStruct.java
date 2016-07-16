/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@link GlobalPackageStruct} is the global location for system defined packages.
 */
@SuppressWarnings("all")
public final class GlobalPackageStruct {

	static final Map<String, PackageStruct> ALL_PACKAGES = new ConcurrentHashMap<>();

	// TODO: Eventually, we must make sure we account for lexical constraints for the CL and Keyword packages defined in the specification.
	public static final PackageStruct EXTENSIONS = new PackageStruct("EXTENSIONS");

	public static final PackageStruct COMPILER = new PackageStruct("COMPILER");

	public static final PackageStruct BACKQUOTE = new PackageStruct("BACKQUOTE");

	public static final PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP", Collections.singletonList("CL"), COMPILER, BACKQUOTE);

	public static final PackageStruct SYSTEM = new PackageStruct("SYSTEM", Collections.emptyList(), COMPILER, BACKQUOTE, COMMON_LISP);

	public static final PackageStruct JCL_TYPE = new PackageStruct("JCL-TYPE");

	public static final PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER", Collections.singletonList("CL-USER"), COMMON_LISP, EXTENSIONS);

	public static final PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;

	private GlobalPackageStruct() {
	}
}
