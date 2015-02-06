/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class LexicalEnvironment extends Environment {

	public static final LexicalEnvironment NULL = new LexicalEnvironment(null, Marker.LAMBDA, 0);

	private static final long serialVersionUID = -7302727063376061009L;

	private final Marker marker;

	private final Closure closure;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();

	public LexicalEnvironment(final Environment parent, final Marker marker, final int closureDepth) {
		super(parent);
		this.marker = marker;
		closure = new Closure(closureDepth);
	}

	public Marker getMarker() {
		return marker;
	}

	public Closure getClosure() {
		return closure;
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public void addLoadTimeValue(final LoadTimeValue loadTimeValue) {
		loadTimeValues.add(loadTimeValue);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
