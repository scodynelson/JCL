/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.compiler.real.environment.binding.SymbolBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class SymbolTable implements Serializable {

	private static final long serialVersionUID = 7149597963303419636L;

	private final List<SymbolBinding<?>> bindings = new ArrayList<>();

	public boolean hasBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<SymbolBinding<?>> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public List<SymbolLocalBinding> getLocalBindings() {
		return bindings.stream()
		               .filter(e -> e instanceof SymbolLocalBinding)
		               .map(SymbolLocalBinding.class::cast)
		               .collect(Collectors.toList());
	}

	public boolean hasLocalBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e instanceof SymbolLocalBinding)
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<SymbolLocalBinding> getLocalBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .filter(e -> e instanceof SymbolLocalBinding)
		               .map(SymbolLocalBinding.class::cast)
		               .findFirst();
	}

	public void addLocalBinding(final SymbolLocalBinding symbolBinding) {
		bindings.add(symbolBinding);
	}

	public List<SymbolEnvironmentBinding> getEnvironmentBindings() {
		return bindings.stream()
		               .filter(e -> e instanceof SymbolEnvironmentBinding)
		               .map(SymbolEnvironmentBinding.class::cast)
		               .collect(Collectors.toList());
	}

	public Optional<SymbolEnvironmentBinding> getEnvironmentBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .filter(e -> e instanceof SymbolEnvironmentBinding)
		               .map(SymbolEnvironmentBinding.class::cast)
		               .findFirst();
	}

	public boolean hasEnvironmentBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e instanceof SymbolEnvironmentBinding)
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public void addEnvironmentBinding(final SymbolEnvironmentBinding symbolBinding) {
		bindings.add(symbolBinding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
