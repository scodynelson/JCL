/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system;

import jcl.pathnames.PathnameFileStruct;
import jcl.pathnames.PathnameVariables;

public class InitializeVariables {

	static {
		PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.setValue(new PathnameFileStruct(""));
	}
}
