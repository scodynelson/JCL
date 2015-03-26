/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LocallyStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 3549306656634788482L;

	private final PrognStruct forms;

	private final LocallyEnvironment locallyEnvironment;

	public LocallyStruct(final PrognStruct forms, final LocallyEnvironment locallyEnvironment) {
		this.forms = forms;
		this.locallyEnvironment = locallyEnvironment;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public LocallyEnvironment getLocallyEnvironment() {
		return locallyEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(forms)
		                            .append(locallyEnvironment)
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
		final LocallyStruct rhs = (LocallyStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(forms, rhs.forms)
		                          .append(locallyEnvironment, rhs.locallyEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(forms)
		                                                                .append(locallyEnvironment)
		                                                                .toString();
	}
}
