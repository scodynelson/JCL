/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.HashMap;
import java.util.Map;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	private final Map<String, LispStruct> loadTimeValues = new HashMap<>();

	private int parameterNumber;

	public LambdaEnvironment(final Environment parent) {
		super(parent);
	}

	public Map<String, LispStruct> getLoadTimeValues() {
		return loadTimeValues;
	}

	public int getNextParameterNumber() {
		return parameterNumber++;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(loadTimeValues)
		                            .append(parameterNumber)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final LambdaEnvironment rhs = (LambdaEnvironment) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(loadTimeValues, rhs.loadTimeValues)
		                          .append(parameterNumber, rhs.parameterNumber)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(loadTimeValues)
		                                                                .append(parameterNumber)
		                                                                .toString();
	}
}
