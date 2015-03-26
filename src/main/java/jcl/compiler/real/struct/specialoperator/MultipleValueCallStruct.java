/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class MultipleValueCallStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 2789725049143539321L;

	private final LispStruct functionForm;

	private final List<LispStruct> forms;

	public MultipleValueCallStruct(final LispStruct functionForm, final List<LispStruct> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public LispStruct getFunctionForm() {
		return functionForm;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(functionForm)
		                            .append(forms)
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
		final MultipleValueCallStruct rhs = (MultipleValueCallStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(functionForm, rhs.functionForm)
		                          .append(forms, rhs.forms)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(functionForm)
		                                                                .append(forms)
		                                                                .toString();
	}
}
