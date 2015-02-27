/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.lambda;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.StringElement;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaElement implements Element {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringElement docString;

	private final Element body;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaElement(final OrdinaryLambdaListBindings lambdaListBindings, final StringElement docString, final Element body,
	                     final LambdaEnvironment lambdaEnvironment) {
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.body = body;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringElement getDocString() {
		return docString;
	}

	public Element getBody() {
		return body;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
