/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.Allocation;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolBinding<A extends Allocation> extends Binding<A> {

	private static final long serialVersionUID = -3462756070576114237L;

	private final Environment binding;

	protected SymbolBinding(final SymbolElement symbolStruct, final A allocation, final LispType type,
	                        final Environment binding) {
		super(symbolStruct, allocation, type);
		this.binding = binding;
	}

	public Environment getBinding() {
		return binding;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
