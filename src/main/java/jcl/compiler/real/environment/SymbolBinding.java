package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolBinding extends Binding {

	private static final long serialVersionUID = -3462756070576114237L;

	private final Environment<?> binding;

	public SymbolBinding(final SymbolStruct<?> symbolStruct, final Allocation allocation, final Scope scope,
	                     final LispType type, final Environment<?> binding) {
		super(symbolStruct, allocation, scope, type);
		this.binding = binding;
	}

	public Environment<?> getBinding() {
		return binding;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
