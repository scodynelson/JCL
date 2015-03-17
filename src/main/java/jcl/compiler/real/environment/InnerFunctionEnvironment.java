/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

public class InnerFunctionEnvironment extends LambdaEnvironment {

	private static final long serialVersionUID = -5882720704455871085L;

	protected InnerFunctionEnvironment(final Environment parent) {
		super(parent);
	}
}
