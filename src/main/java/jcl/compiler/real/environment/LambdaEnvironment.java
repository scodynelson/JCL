/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	private int parameterNumber;

	public LambdaEnvironment(final Environment parent, final int closureDepth) {
		super(parent, closureDepth);
	}

	public int getNextParameterNumber() {
		return parameterNumber++;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).setExcludeFieldNames("parent").toString();
	}
}
