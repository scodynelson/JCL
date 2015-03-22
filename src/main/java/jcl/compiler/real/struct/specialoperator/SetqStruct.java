/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SetqStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -5092653942359022766L;

	private final List<SetqPair> setqPairs;

	public SetqStruct(final List<SetqPair> setqPairs) {
		this.setqPairs = setqPairs;
	}

	public List<SetqPair> getSetqPairs() {
		return setqPairs;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(setqPairs)
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
		final SetqStruct rhs = (SetqStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(setqPairs, rhs.setqPairs)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(setqPairs)
		                                                                .toString();
	}

	public static class SetqPair implements Serializable {

		private static final long serialVersionUID = -7804939280136663517L;

		private final SymbolStruct<?> var;

		private final LispStruct form;

		public SetqPair(final SymbolStruct<?> var, final LispStruct form) {
			this.var = var;
			this.form = form;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getForm() {
			return form;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(var)
			                            .append(form)
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
			final SetqPair rhs = (SetqPair) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(form, rhs.form)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .append(form)
			                                                                .toString();
		}
	}
}
