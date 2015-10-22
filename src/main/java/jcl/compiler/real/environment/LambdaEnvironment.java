/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.HashMap;
import java.util.Map;

import jcl.LispStruct;

public class LambdaEnvironment extends Environment {

	private static final long serialVersionUID = -1182568685360839544L;

	private final Map<String, LispStruct> loadTimeValues = new HashMap<>();

	public LambdaEnvironment(final Environment parent) {
		super(parent);
	}

	public Map<String, LispStruct> getLoadTimeValues() {
		return loadTimeValues;
	}
}
