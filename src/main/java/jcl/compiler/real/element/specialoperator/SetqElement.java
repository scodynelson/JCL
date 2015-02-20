/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.List;

public class SetqElement implements Element {

	private static final long serialVersionUID = -5092653942359022766L;

	private final List<SetqPair> setqPairs;

	public SetqElement(final List<SetqPair> setqPairs) {
		this.setqPairs = setqPairs;
	}

	public List<SetqPair> getSetqPairs() {
		return setqPairs;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	public static class SetqPair implements Serializable {

		private static final long serialVersionUID = -7804939280136663517L;

		private final SymbolElement<?> var;
		private final Element form;

		public SetqPair(final SymbolElement<?> var, final Element form) {
			this.var = var;
			this.form = form;
		}

		public SymbolElement<?> getVar() {
			return var;
		}

		public Element getForm() {
			return form;
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
