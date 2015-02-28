/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.List;

public class ProgvElement implements Element {

	private static final long serialVersionUID = 6286708668973616872L;

	private final List<ProgvVar> vars;

	private final List<Element> forms;

	private final List<Binding<?>> previousBindings;

	private final Environment environment;

	public ProgvElement(final List<ProgvVar> vars, final List<Element> forms, final List<Binding<?>> previousBindings,
	                    final Environment environment) {
		this.vars = vars;
		this.forms = forms;
		this.previousBindings = previousBindings;
		this.environment = environment;
	}

	public List<ProgvVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public List<Binding<?>> getPreviousBindings() {
		return previousBindings;
	}

	public Environment getEnvironment() {
		return environment;
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
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	public static class ProgvVar implements Serializable {

		private static final long serialVersionUID = -5131005121770228469L;

		private final SymbolElement var;

		private final Element val;

		public ProgvVar(final SymbolElement var, final Element val) {
			this.var = var;
			this.val = val;
		}

		public SymbolElement getVar() {
			return var;
		}

		public Element getVal() {
			return val;
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
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
