/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import jcl.functions.AbstractCommonLispFunctionStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1982336595153324433L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public MakePackageFunction() {
		super("");
	}

	@Override
	protected String functionName() {
		return "MAKE-PACKAGE";
	}
}
