/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class LexicalEnvironment extends Environment<LexicalEnvironment> {

	public static final LexicalEnvironment NULL = new LexicalEnvironment(null, Marker.LAMBDA, 0);

	private static final long serialVersionUID = -7302727063376061009L;

	private final Marker marker;

	private final Closure environmentClosure;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();

	// TODO: load-time-value ???
	public LexicalEnvironment(final LexicalEnvironment parent, final Marker marker, final int closureDepth) {
		super(parent);
		this.marker = marker;
		environmentClosure = new Closure(closureDepth);
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public Marker getMarker() {
		return marker;
	}

	public Closure getEnvironmentClosure() {
		return environmentClosure;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
