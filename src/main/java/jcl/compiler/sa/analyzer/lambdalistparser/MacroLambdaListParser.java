/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class MacroLambdaListParser {

	public static MacroLambdaList parseMacroLambdaList(final Environment environment, final ListStruct lambdaList,
	                                            final DeclareStruct declareElement) {

		final LispStruct last = lambdaList.last(IntegerStruct.ZERO);
		if (NILStruct.INSTANCE.eq(last)) {
			return getLambdaListBindings(environment, lambdaList, declareElement);
		} else {
			return getDottedLambdaListBindings(environment, lambdaList, declareElement);
		}
	}

	private static MacroLambdaList getLambdaListBindings(final Environment environment, final ListStruct lambdaList,
	                                                     final DeclareStruct declareElement) {

		final Iterator<LispStruct> iterator = lambdaList.iterator();

		// NOTE: this first peek is to ensure we don't damage the initial state of the iterator by consuming the first element early.
		LispStruct firstElement = null;
		if (!NILStruct.INSTANCE.eq(lambdaList)) {
			firstElement = lambdaList.car();
		}

		LispStruct currentElement = null;

		WholeParameter wholeBinding = null;
		if (CommonLispSymbols.AND_WHOLE.eq(firstElement)) {
			// Now that we've verified the first element is actually '&whole', consume it.
			currentElement = iterator.next();

			final WholeParseResult wholeParseResult
					= LambdaListParser.parseWholeBinding(environment, iterator, declareElement);

			wholeBinding = wholeParseResult.getWholeBinding();
		}

		EnvironmentParameter environmentBinding = null;
		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, false);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
		}

		List<RequiredParameter> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= LambdaListParser.parseRequiredBindings(environment, iterator, declareElement, false, true);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		List<OptionalParameter> optionalBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_OPTIONAL.eq(currentElement)) {
			final OptionalParseResult optionalParseResult
					= LambdaListParser.parseOptionalBindings(environment, iterator, declareElement, false, true);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		RestParameter restBinding = null;
		if (CommonLispSymbols.AND_REST.eq(currentElement)) {
			final RestParseResult restParseResult
					= LambdaListParser.parseRestBinding(environment, iterator, declareElement, true);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
		}

		BodyParameter bodyBinding = null;
		if (CommonLispSymbols.AND_BODY.eq(currentElement)) {
			if (restBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &body parameter cannot be supplied alongside &rest parameter.");
			}

			final BodyParseResult bodyParseResult
					= LambdaListParser.parseBodyBinding(environment, iterator, declareElement, true);

			bodyBinding = bodyParseResult.getBodyBinding();
			currentElement = bodyParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		boolean keyNotProvided = true;

		List<KeyParameter> keyBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_KEY.eq(currentElement)) {
			final KeyParseResult keyParseResult
					= LambdaListParser.parseKeyBindings(environment, iterator, declareElement, true);

			keyBindings = keyParseResult.getKeyBindings();
			currentElement = keyParseResult.getCurrentElement();

			keyNotProvided = false;
		}

		boolean allowOtherKeys = false;
		if (CommonLispSymbols.AND_ALLOW_OTHER_KEYS.eq(currentElement)) {
			if (keyNotProvided) {
				throw new ProgramErrorException("&allow-other-keys cannot be provided when &key is not provided.");
			}

			allowOtherKeys = true;
			if (iterator.hasNext()) {
				currentElement = iterator.next();
			}
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		List<AuxParameter> auxBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_AUX.eq(currentElement)) {
			final AuxParseResult auxParseResult
					= LambdaListParser.parseAuxBindings(environment, iterator, declareElement, true);

			auxBindings = auxParseResult.getAuxBindings();
			currentElement = auxParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
		}

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			throw new ProgramErrorException("Unexpected element at the end of Macro Lambda List: " + element);
		}

		return new MacroLambdaList(wholeBinding, environmentBinding, requiredBindings, optionalBindings, restBinding, bodyBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	private static MacroLambdaList getDottedLambdaListBindings(final Environment environment,
	                                                           final ListStruct lambdaList,
	                                                           final DeclareStruct declareElement) {

		final Iterator<LispStruct> iterator = lambdaList.iterator();

		// NOTE: this first peek is to ensure we don't damage the initial state of the iterator by consuming the first element early.
		LispStruct firstElement = null;
		if (!NILStruct.INSTANCE.eq(lambdaList)) {
			firstElement = lambdaList.car();
		}

		LispStruct currentElement = null;

		WholeParameter wholeBinding = null;
		if (CommonLispSymbols.AND_WHOLE.eq(firstElement)) {
			// Now that we've verified the first element is actually '&whole', consume it.
			currentElement = iterator.next();

			final WholeParseResult wholeParseResult
					= LambdaListParser.parseWholeBinding(environment, iterator, declareElement);

			wholeBinding = wholeParseResult.getWholeBinding();
		}

		EnvironmentParameter environmentBinding = null;
		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, false);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
		}

		List<RequiredParameter> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= LambdaListParser.parseRequiredBindings(environment, iterator, declareElement, true, true);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		List<OptionalParameter> optionalBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_OPTIONAL.eq(currentElement)) {
			final OptionalParseResult optionalParseResult
					= LambdaListParser.parseOptionalBindings(environment, iterator, declareElement, true, true);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
		}

		if (CommonLispSymbols.AND_ENVIRONMENT.eq(currentElement)) {
			if (environmentBinding != null) {
				throw new ProgramErrorException("Macro LambdaList &environment parameter cannot be supplied twice.");
			}

			final EnvironmentParseResult environmentParseResult
					= LambdaListParser.parseEnvironmentBinding(environment, iterator, true);

			environmentBinding = environmentParseResult.getEnvironmentBinding();
			currentElement = environmentParseResult.getCurrentElement();
		}

		final RestParseResult restParseResult
				= LambdaListParser.parseDottedRestBinding(environment, currentElement, declareElement, true);
		final RestParameter restBinding = restParseResult.getRestBinding();

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			throw new ProgramErrorException("Unexpected element at the end of Destructuring Lambda List: " + element);
		}

		return new MacroLambdaList(wholeBinding, environmentBinding, requiredBindings, optionalBindings, restBinding, null, Collections.emptyList(), Collections.emptyList(), false);
	}
}
