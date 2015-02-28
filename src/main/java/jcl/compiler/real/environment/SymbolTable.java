/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.binding.SymbolClosureBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class SymbolTable implements Serializable {

	private static final long serialVersionUID = 7149597963303419636L;

	private final List<SymbolEnvironmentBinding> lexicalEnvironmentBindings = new ArrayList<>();

	private final List<SymbolClosureBinding> closureBindings = new ArrayList<>();

	private final List<SymbolEnvironmentBinding> dynamicEnvironmentBindings = new ArrayList<>();

	private final List<SymbolLocalBinding> dynamicLocalBindings = new ArrayList<>();

	public boolean hasBinding(final SymbolElement symbolStruct) {
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

	public boolean hasDynamicBinding(final SymbolElement symbolStruct) {
		return dynamicEnvironmentBindings.stream()
		                                 .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct))
				||
				dynamicLocalBindings.stream()
				                    .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public boolean hasClosureBinding(final SymbolElement symbolStruct) {
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

	public Optional<SymbolEnvironmentBinding> getLexicalEnvironmentBinding(final SymbolElement symbolStruct) {
		return lexicalEnvironmentBindings.stream()
		                                 .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                                 .findFirst();
	}

	public Optional<SymbolClosureBinding> getClosureBinding(final SymbolElement symbolStruct) {
		return closureBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public Optional<SymbolEnvironmentBinding> getDynamicEnvironmentBinding(final SymbolElement symbolStruct) {
		return dynamicEnvironmentBindings.stream()
		                                 .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                                 .findFirst();
	}

	public Optional<SymbolLocalBinding> getDynamicLocalBinding(final SymbolElement symbolStruct) {
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
