/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.sa.Analyzer;
import jcl.conditions.exceptions.ProgramErrorException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class FunctionCallAnalyzer implements Analyzer<Element, ConsElement> {

	private static final long serialVersionUID = 5048632787143001075L;

	protected static void validateFunctionArguments(final String functionName, final OrdinaryLambdaListBindings lambdaListBindings,
	                                                final List<SimpleElement> functionArguments) {

		final Iterator<SimpleElement> functionArgumentsIterator = functionArguments.iterator();

		SimpleElement nextArgument = null;

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding ignored : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("LIST ANALYZER: Too few arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at least " + requiredBindings.size() + " required.");
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding ignored : optionalBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				break;
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		final List<SymbolElement> keys = new ArrayList<>(keyBindings.size());
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolElement key = keyBinding.getKeyName();
			keys.add(key);
		}

		final RestBinding restBinding = lambdaListBindings.getRestBinding();

		while (functionArgumentsIterator.hasNext()) {
			if (!keyBindings.isEmpty()) {
				if (nextArgument instanceof SymbolElement) {
					final SymbolElement argumentKey = (SymbolElement) nextArgument;
					if (keys.contains(argumentKey)) {
						// Consume the next argument
						functionArgumentsIterator.next();
						nextArgument = functionArgumentsIterator.next();
					} else {
						throw new ProgramErrorException("LIST ANALYZER: Keyword argument not found in '" + functionName + "' function definition: " + argumentKey);
					}
				} else {
					throw new ProgramErrorException("LIST ANALYZER: Expected Keyword argument for call to '" + functionName + " was: " + nextArgument);
				}
			} else if (restBinding != null) {
				functionArgumentsIterator.next();
			} else {
				throw new ProgramErrorException("LIST ANALYZER: Too many arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at most " + requiredBindings.size() + " accepted.");
			}
		}
	}
}
