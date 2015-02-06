/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.EnumSet;
import java.util.Set;

public enum Marker {

	LAMBDA,
	MACROLET,
	LET,
	FLET,
	LABELS,
	LOCALLY,
	SYMBOL_MACROLET;

	public static final Set<Marker> BINDING_MARKERS = EnumSet.of(LAMBDA, LET, MACROLET);

	public static final Set<Marker> LAMBDA_MARKERS = EnumSet.of(LAMBDA, FLET, LABELS);

	public static final Set<Marker> FUNCTION_MARKERS = EnumSet.of(FLET, LABELS);

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}


	/*

	locally - lexical, environment

	let - variable-binding, lexical, environment

	progv - [variable-binding], [dynamic], environment

	lambda - variable-binding, lambda, lexical, environment

	flet - function-binding, lambda, lexical, environment
	labels - function-binding, lambda, lexical, environment

	macrolet - macro-binding, function-binding, lexical, environment

	symbol-macrolet - symbol-macro-binding, variable-binding, lexical, environment

	 */
}
