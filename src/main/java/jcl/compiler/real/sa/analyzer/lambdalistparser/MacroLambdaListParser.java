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
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.BodyBinding;
import jcl.compiler.real.environment.binding.lambdalist.EnvironmentBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.WholeBinding;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MacroLambdaListParser extends LambdaListParser {

	@Autowired
	private DestructuringLambdaListParser destructuringLambdaListParser;

	@Autowired
	private Printer printer;

	public MacroLambdaListBindings parseMacroLambdaList(final Environment environment, final ListStruct lambdaList,
	                                                    final DeclareStruct declareElement) {

		if (lambdaList.isDotted()) {
			return getDottedLambdaListBindings(environment, lambdaList, declareElement);
		} else {
			return getLambdaListBindings(environment, lambdaList, declareElement);
		}
	}

	private MacroLambdaListBindings getLambdaListBindings(final Environment environment, final ListStruct lambdaList,
	                                                      final DeclareStruct declareElement) {

		final List<LispStruct> lambdaListJava = lambdaList.getAsJavaList();

		final Iterator<LispStruct> iterator = lambdaListJava.iterator();

		// NOTE: this first peek is to ensure we don't damage the initial state of the iterator by consuming the first element early.
		LispStruct firstElement = null;
		if (!lambdaListJava.isEmpty()) {
			firstElement = lambdaListJava.get(0);
		}

		LispStruct currentElement = null;
		int position = 0;

		WholeBinding wholeBinding = null;
		if (CompilerConstants.WHOLE.equals(firstElement)) {
			// Now that we've verified the first element is actually '&whole', consume it.
			currentElement = iterator.next();

			final WholeParseResult wholeParseResult
					= parseWholeBinding(environment, iterator, position, declareElement);

			wholeBinding = wholeParseResult.getWholeBinding();
			position = wholeParseResult.getCurrentPosition();
		}

		EnvironmentBinding environmentBinding = null;
		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, false);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			position = environmentParseResult.getCurrentPosition();
		}

		List<RequiredBinding> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= parseRequiredBindings(environment, iterator, position, declareElement, false);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
			position = requiredParseResult.getCurrentPosition();
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
		}

		List<OptionalBinding> optionalBindings = Collections.emptyList();
		if (CompilerConstants.OPTIONAL.equals(currentElement)) {
			final OptionalParseResult optionalParseResult
					= parseOptionalBindings(environment, iterator, position, declareElement, false);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
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

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
		}

		boolean keyNotProvided = true;

		List<KeyBinding> keyBindings = Collections.emptyList();
		if (CompilerConstants.KEY.equals(currentElement)) {
			final KeyParseResult keyParseResult
					= parseKeyBindings(environment, iterator, position, declareElement);

			keyBindings = keyParseResult.getKeyBindings();
			currentElement = keyParseResult.getCurrentElement();
			position = keyParseResult.getCurrentPosition();

			keyNotProvided = false;
		}

		boolean allowOtherKeys = false;
		if (CompilerConstants.ALLOW_OTHER_KEYS.equals(currentElement)) {
			if (keyNotProvided) {
				throw new ProgramErrorException("&allow-other-keys cannot be provided when &key is not provided.");
			}

			allowOtherKeys = true;
			if (iterator.hasNext()) {
				currentElement = iterator.next();
			}
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
		}

		List<AuxBinding> auxBindings = Collections.emptyList();
		if (CompilerConstants.AUX.equals(currentElement)) {
			final AuxParseResult auxParseResult
					= parseAuxBindings(environment, iterator, position, declareElement);

			auxBindings = auxParseResult.getAuxBindings();
			currentElement = auxParseResult.getCurrentElement();
			position = auxParseResult.getCurrentPosition();
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
		}

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			final String printedElement = printer.print(element);
			throw new ProgramErrorException("Unexpected element at the end of Macro Lambda List: " + printedElement);
		}

		return new MacroLambdaListBindings(wholeBinding, environmentBinding, requiredBindings, optionalBindings, restBinding, bodyBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	private MacroLambdaListBindings getDottedLambdaListBindings(final Environment environment, final ListStruct lambdaList,
	                                                            final DeclareStruct declareElement) {

		final List<LispStruct> lambdaListJava = lambdaList.getAsJavaList();

		final Iterator<LispStruct> iterator = lambdaListJava.iterator();

		// NOTE: this first peek is to ensure we don't damage the initial state of the iterator by consuming the first element early.
		LispStruct firstElement = null;
		if (!lambdaListJava.isEmpty()) {
			firstElement = lambdaListJava.get(0);
		}

		LispStruct currentElement = null;
		int position = 0;

		WholeBinding wholeBinding = null;
		if (CompilerConstants.WHOLE.equals(firstElement)) {
			// Now that we've verified the first element is actually '&whole', consume it.
			currentElement = iterator.next();

			final WholeParseResult wholeParseResult
					= parseWholeBinding(environment, iterator, position, declareElement);

			wholeBinding = wholeParseResult.getWholeBinding();
			position = wholeParseResult.getCurrentPosition();
		}

		EnvironmentBinding environmentBinding = null;
		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, false);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			position = environmentParseResult.getCurrentPosition();
		}

		List<RequiredBinding> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= parseRequiredBindings(environment, iterator, position, declareElement, true);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
			position = requiredParseResult.getCurrentPosition();
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
		}

		List<OptionalBinding> optionalBindings = Collections.emptyList();
		if (CompilerConstants.OPTIONAL.equals(currentElement)) {
			final OptionalParseResult optionalParseResult
					= parseOptionalBindings(environment, iterator, position, declareElement, true);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		if (CompilerConstants.ENVIRONMENT.equals(currentElement)) {
			if (environment != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= parseEnvironmentBinding(environment, iterator, position, declareElement, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
			position = environmentParseResult.getCurrentPosition();
		}

		final RestParseResult restParseResult
				= parseDottedRestBinding(environment, currentElement, position, declareElement);
		final RestBinding restBinding = restParseResult.getRestBinding();

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			final String printedElement = printer.print(element);
			throw new ProgramErrorException("Unexpected element at the end of Destructuring Lambda List: " + printedElement);
		}

		return new MacroLambdaListBindings(wholeBinding, environmentBinding, requiredBindings, optionalBindings, restBinding, null, Collections.emptyList(), Collections.emptyList(), false);
	}
}