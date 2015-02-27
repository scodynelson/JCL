/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class KeyBinding extends ParameterBinding {

	private static final long serialVersionUID = -8247621420473541525L;

	private final SymbolElement keyName;

	private final SuppliedPBinding suppliedPBinding;

	public KeyBinding(final SymbolElement symbolStruct, final ParameterAllocation allocation, final SimpleElement initForm,
	                  final SymbolElement keyName, final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocation, T.INSTANCE, initForm);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public SymbolElement getKeyName() {
		return keyName;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
