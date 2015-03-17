/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.List;

import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public final class Environments {

	private Environments() {
	}

	public static Environment getDynamicBindingEnvironment(final Environment environment, final SymbolStruct<?> var) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);
			if (hasDynamicBinding) {
				break;
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}

	public static Environment getInnerFunctionLexicalBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment instanceof InnerFunctionEnvironment) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}

	public static Environment getFunctionLexicalBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment instanceof LambdaEnvironment) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}

	public static BindingEnvironment getDynamicBindingBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment instanceof BindingEnvironment) {

				final boolean hasBinding = currentEnvironment.hasDynamicBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		// NOTE: This will never be an improper cast, since the Null Environment is a BindingEnvironment
		return (BindingEnvironment) currentEnvironment;
	}

	public static BindingEnvironment getLexicalBindingBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment instanceof BindingEnvironment) {

				final boolean hasBinding = currentEnvironment.hasLexicalBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		// NOTE: This will never be an improper cast, since the Null Environment is a BindingEnvironment
		return (BindingEnvironment) currentEnvironment;
	}

	public static LambdaEnvironment getEnclosingLambda(final Environment environment) {

		Environment currentEnvironment = environment;

		while (!(currentEnvironment instanceof LambdaEnvironment)) {
			currentEnvironment = currentEnvironment.getParent();
		}

		// NOTE: This will never be an improper cast, since the Null Environment is a LambdaEnvironment
		return (LambdaEnvironment) currentEnvironment;
	}

	public static boolean hasFunctionBinding(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		boolean hasFunctionBinding = false;

		while (!currentEnvironment.equals(Environment.NULL)) {
			if (currentEnvironment.hasLexicalBinding(variable)) {
				hasFunctionBinding = true;
				break;
			}
			currentEnvironment = currentEnvironment.getParent();
		}

		return hasFunctionBinding;
	}

	public static void addDynamicVariableBinding(final SpecialDeclarationStruct specialDeclarationElement,
	                                              final Environment environment) {

		final LambdaEnvironment currentLambda = getEnclosingLambda(environment);
		final int nextBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(nextBindingsPosition);

		final SymbolStruct<?> var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = getDynamicBindingEnvironment(environment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		environment.addDynamicBinding(binding);
	}

	public static boolean isSpecial(final DeclareStruct declareElement, final SymbolStruct<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationStruct specialDeclarationElement : specialDeclarationElements) {
			final SymbolStruct<?> specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}
}
