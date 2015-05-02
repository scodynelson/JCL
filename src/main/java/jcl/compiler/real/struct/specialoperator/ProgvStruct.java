/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ProgvStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 6286708668973616872L;

	private final LispStruct vars;

	private final LispStruct vals;

	private final PrognStruct forms;

	private final ProgvEnvironment progvEnvironment;

	public ProgvStruct(final LispStruct vars, final LispStruct vals, final PrognStruct forms, final ProgvEnvironment progvEnvironment) {
		this.vars = vars;
		this.vals = vals;
		this.forms = forms;
		this.progvEnvironment = progvEnvironment;
	}

	public LispStruct getVars() {
		return vars;
	}

	public LispStruct getVals() {
		return vals;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public ProgvEnvironment getProgvEnvironment() {
		return progvEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(vals)
		                            .append(forms)
		                            .append(progvEnvironment)
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
		final ProgvStruct rhs = (ProgvStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(vals, rhs.vals)
		                          .append(forms, rhs.forms)
		                          .append(progvEnvironment, rhs.progvEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(vals)
		                                                                .append(forms)
		                                                                .append(progvEnvironment)
		                                                                .toString();
	}
}
