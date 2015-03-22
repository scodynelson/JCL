/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();

	private int parameterNumber;

	public LambdaEnvironment(final Environment parent) {
		super(parent);
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public void addLoadTimeValue(final LoadTimeValue loadTimeValue) {
		loadTimeValues.add(loadTimeValue);
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
