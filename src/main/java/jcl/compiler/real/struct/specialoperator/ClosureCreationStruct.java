/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class ClosureCreationStruct<V> extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 9216281797404250223L;

	private final List<V> vars;

	private final PrognStruct forms;

	private final Environment environment;

	protected ClosureCreationStruct(final List<V> vars, final PrognStruct forms, final Environment environment) {
		this.vars = vars;
		this.forms = forms;
		this.environment = environment;
	}

	public List<V> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getEnvironment() {
		return environment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(environment)
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
		final ClosureCreationStruct<?> rhs = (ClosureCreationStruct<?>) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(environment, rhs.environment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(environment)
		                                                                .toString();
	}
}
