/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.PackageStruct;
import jcl.lang.internal.VariableStructImpl;

/**
 * Defines the standard package variables for the system.
 */
@SuppressWarnings("all")
public final class PackageVariables {

	// Package Variable

	public static final VariableStructImpl<PackageStruct> PACKAGE = VariableStructImpl.valueOf("*PACKAGE*", GlobalPackageStruct.COMMON_LISP, GlobalPackageStruct.COMMON_LISP_USER);

	private PackageVariables() {
	}
}
