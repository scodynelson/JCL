/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class Environment implements LispStruct {

	public static final Environment NULL = new Environment(null, Marker.LAMBDA, 0);

	private static final long serialVersionUID = 7523547599975901124L;

	private final Environment parent;

	private final List<EnvironmentParameterBinding> lexicalBindings = new ArrayList<>();

	private final List<EnvironmentBinding<?>> dynamicBindings = new ArrayList<>();

	private final SymbolTable symbolTable = new SymbolTable();

	private final Marker marker;

	private final Closure closure;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();

	public static final Set<Class<? extends Environment>> BINDING_ENVIRONMENTS = new HashSet<>(Arrays.asList(LambdaEnvironment.class, LetEnvironment.class, MacroletEnvironment.class));

	protected Environment(final Environment parent, final Marker marker, final int closureDepth) {
		this.parent = parent;
		this.marker = marker;
		closure = new Closure(closureDepth);
	}

	public Environment getParent() {
		return parent;
	}

	public List<EnvironmentParameterBinding> getLexicalBindings() {
		return lexicalBindings;
	}

	public boolean hasLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentParameterBinding> getLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addLexicalBinding(final EnvironmentParameterBinding environmentBinding) {
		lexicalBindings.add(environmentBinding);
	}

	public List<EnvironmentBinding<?>> getDynamicBindings() {
		return dynamicBindings;
	}

	public boolean hasDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentBinding<?>> getDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addDynamicBinding(final EnvironmentBinding<?> environmentBinding) {
		dynamicBindings.add(environmentBinding);
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	public Marker getMarker() {
		return marker;
	}

	public Closure getClosure() {
		return closure;
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public void addLoadTimeValue(final LoadTimeValue loadTimeValue) {
		loadTimeValues.add(loadTimeValue);
	}

	public static Environment getDynamicBindingEnvironment(final Environment environment, final SymbolStruct<?> var) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(NULL)) {

			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);
			if (hasDynamicBinding) {
				break;
			}

			currentEnvironment = currentEnvironment.parent;
		}

		return currentEnvironment;
	}

	public static Environment getLexicalBindingEnvironment(final Environment environment, final SymbolStruct<?> variable,
	                                                       final Set<Class<? extends Environment>> environmentTypes) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(NULL)) {

			if (environmentTypes.contains(currentEnvironment.getClass())) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.parent;
		}

		return currentEnvironment;
	}

	public static Environment getInnerFunctionBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(NULL)) {

			if (currentEnvironment instanceof InnerFunctionEnvironment) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.parent;
		}

		return currentEnvironment;
	}

	public static Environment getFunctionBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(NULL)) {

			if (currentEnvironment instanceof FunctionEnvironment) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.parent;
		}

		return currentEnvironment;
	}

	public static Environment getEnclosingLambda(final Environment environment) {

		Environment currentEnvironment = environment;

		while (!(currentEnvironment instanceof FunctionEnvironment)) {
			currentEnvironment = currentEnvironment.parent;
		}

		return currentEnvironment;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
