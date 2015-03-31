/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.CompilerConstants;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.BodyBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MacroLambdaListParser extends LambdaListParser {

	@Autowired
	private Printer printer;

	public MacroLambdaListBindings parseMacroLambdaList(final Environment environment, final ListStruct lambdaList,
	                                                       final DeclareStruct declareElement) {

		final List<? extends LispStruct> lambdaListJava = lambdaList.getAsJavaList();

		final Iterator<? extends LispStruct> iterator = lambdaListJava.iterator();

		LispStruct currentElement = null;
		int position = 0;

		List<RequiredBinding> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= parseRequiredBindings(environment, iterator, position, declareElement);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
			position = requiredParseResult.getCurrentPosition();
		}

		List<OptionalBinding> optionalBindings = Collections.emptyList();
		if (CompilerConstants.OPTIONAL.equals(currentElement)) {
			final OptionalParseResult optionalParseResult
					= parseOptionalBindings(environment, iterator, position, declareElement);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		RestBinding restBinding = null;
		if (CompilerConstants.REST.equals(currentElement)) {
			final RestParseResult restParseResult
					= parseRestBinding(environment, iterator, position, declareElement);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
			position = restParseResult.getCurrentPosition();
		}

		BodyBinding bodyBinding = null;
		if (CompilerConstants.BODY.equals(currentElement)) {
			if (restBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &body parameter cannot be supplied alongside &rest parameter.");
			}

			final BodyParseResult bodyParseResult
					= parseBodyBinding(environment, iterator, position, declareElement);

			bodyBinding = bodyParseResult.getBodyBinding();
			currentElement = bodyParseResult.getCurrentElement();
			position = bodyParseResult.getCurrentPosition();
		}

		List<KeyBinding> keyBindings = Collections.emptyList();
		if (CompilerConstants.KEY.equals(currentElement)) {
			final KeyParseResult keyParseResult
					= parseKeyBindings(environment, iterator, position, declareElement);

			keyBindings = keyParseResult.getKeyBindings();
			currentElement = keyParseResult.getCurrentElement();
			position = keyParseResult.getCurrentPosition();
		}

		boolean allowOtherKeys = false;
		if (CompilerConstants.ALLOW_OTHER_KEYS.equals(currentElement)) {
			allowOtherKeys = true;
			if (iterator.hasNext()) {
				currentElement = iterator.next();
			}
		}

		List<AuxBinding> auxBindings = Collections.emptyList();
		if (CompilerConstants.AUX.equals(currentElement)) {
			final AuxParseResult auxParseResult
					= parseAuxBindings(environment, iterator, position, declareElement);

			auxBindings = auxParseResult.getAuxBindings();
		}

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			final String printedElement = printer.print(element);
			throw new ProgramErrorException("Unexpected element at the end of Macro Lambda List: " + printedElement);
		}

		return new MacroLambdaListBindings(null, requiredBindings, optionalBindings, restBinding, bodyBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	private WholeParseResult parseWholeBinding() {
		return new WholeParseResult(null, 0, null);
	}

	private BodyParseResult parseBodyBinding(final Environment environment, final Iterator<? extends LispStruct> iterator,
	                                           final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			final String printedElement = printer.print(currentElement);
			throw new ProgramErrorException("LambdaList body parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		if (iterator.hasNext()) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList body parameter must only have 1 parameter: " + printedElement);
			}
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final ParameterAllocation restAllocation = new ParameterAllocation(currentPosition++);
		final BodyBinding bodyBinding = new BodyBinding(currentParam, restAllocation);
		return new BodyParseResult(currentElement, currentPosition, bodyBinding);
	}
}
