/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

public class LetEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -6810260653724112416L;

	public LetEnvironment(final Environment parent) {
		super(parent);
	}
}
