/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LabelsElement.LabelsVar;
import jcl.compiler.real.environment.LabelsEnvironment;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class LabelsElement extends InnerFunctionElement<LabelsEnvironment, LabelsVar> {

	private static final long serialVersionUID = -2347494500321073144L;

	public LabelsElement(final List<LabelsVar> vars, final List<Element> forms, final LabelsEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	public static class LabelsVar extends InnerFunctionElement.InnerFunctionVar {

		private static final long serialVersionUID = 2989214415282349607L;

		public LabelsVar(final SymbolElement<?> var, final Element initForm) {
			super(var, initForm);
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
