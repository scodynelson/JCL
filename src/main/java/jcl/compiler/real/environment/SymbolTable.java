/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import jcl.compiler.real.environment.binding.SymbolClosureBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolTable implements Serializable {

	private static final long serialVersionUID = 7149597963303419636L;

	private final List<SymbolEnvironmentBinding> lexicalEnvironmentBindings = new ArrayList<>();

	private final List<SymbolClosureBinding> closureBindings = new ArrayList<>();

	private final List<SymbolEnvironmentBinding> dynamicEnvironmentBindings = new ArrayList<>();

	private final List<SymbolLocalBinding> dynamicLocalBindings = new ArrayList<>();

	public boolean hasBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalEnvironmentBindings.stream()
		                                 .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct))
				||
				closureBindings.stream()
				               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct))
				||
				dynamicEnvironmentBindings.stream()
				                          .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct))
				||
				dynamicLocalBindings.stream()
				                    .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public boolean hasDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicEnvironmentBindings.stream()
		                                 .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct))
				||
				dynamicLocalBindings.stream()
				                    .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public boolean hasClosureBinding(final SymbolStruct<?> symbolStruct) {
		return closureBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public List<SymbolEnvironmentBinding> getLexicalEnvironmentBindings() {
		return lexicalEnvironmentBindings;
	}

	public List<SymbolClosureBinding> getClosureBindings() {
		return closureBindings;
	}

	public List<SymbolEnvironmentBinding> getDynamicEnvironmentBindings() {
		return dynamicEnvironmentBindings;
	}

	public List<SymbolLocalBinding> getDynamicLocalBindings() {
		return dynamicLocalBindings;
	}

	public Optional<SymbolEnvironmentBinding> getLexicalEnvironmentBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalEnvironmentBindings.stream()
		                                 .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                                 .findFirst();
	}

	public Optional<SymbolClosureBinding> getClosureBinding(final SymbolStruct<?> symbolStruct) {
		return closureBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public Optional<SymbolEnvironmentBinding> getDynamicEnvironmentBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicEnvironmentBindings.stream()
		                                 .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                                 .findFirst();
	}

	public Optional<SymbolLocalBinding> getDynamicLocalBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicLocalBindings.stream()
		                           .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                           .findFirst();
	}

	public void addClosureBinding(final SymbolClosureBinding symbolBinding) {
		closureBindings.add(symbolBinding);
	}

	public void addLexicalEnvironmentBinding(final SymbolEnvironmentBinding symbolBinding) {
		lexicalEnvironmentBindings.add(symbolBinding);
	}

	public void addDynamicEnvironmentBinding(final SymbolEnvironmentBinding symbolBinding) {
		dynamicEnvironmentBindings.add(symbolBinding);
	}

	public void addDynamicLocalBinding(final SymbolLocalBinding symbolBinding) {
		dynamicLocalBindings.add(symbolBinding);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(lexicalEnvironmentBindings)
		                            .append(closureBindings)
		                            .append(dynamicEnvironmentBindings)
		                            .append(dynamicLocalBindings)
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
		final SymbolTable rhs = (SymbolTable) obj;
		return new EqualsBuilder().append(lexicalEnvironmentBindings, rhs.lexicalEnvironmentBindings)
		                          .append(closureBindings, rhs.closureBindings)
		                          .append(dynamicEnvironmentBindings, rhs.dynamicEnvironmentBindings)
		                          .append(dynamicLocalBindings, rhs.dynamicLocalBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(lexicalEnvironmentBindings)
		                                                                .append(closureBindings)
		                                                                .append(dynamicEnvironmentBindings)
		                                                                .append(dynamicLocalBindings)
		                                                                .toString();
	}
}
