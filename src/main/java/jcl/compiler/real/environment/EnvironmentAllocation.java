/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

public class EnvironmentAllocation implements Allocation {

	private Environment environment;

	public EnvironmentAllocation(final Environment environment) {
		this.environment = environment;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public void setEnvironment(final Environment environment) {
		this.environment = environment;
	}
}
