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
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
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
		@SuppressWarnings("checkstyle:strictduplicatecodecheck")
		public int hashCode() {
			return HashCodeBuilder.reflectionHashCode(this);
		}

		@Override
		@SuppressWarnings("checkstyle:strictduplicatecodecheck")
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj);
		}

		@Override
		@SuppressWarnings("checkstyle:strictduplicatecodecheck")
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
