/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class OrdinaryLambdaListParser {

	public static OrdinaryLambdaList parseOrdinaryLambdaList(final Environment environment, final ListStruct lambdaList,
	                                                         final DeclareStruct declareElement) {

		final Iterator<LispStruct> iterator = lambdaList.iterator();

		LispStruct currentElement = null;

		List<RequiredParameter> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= LambdaListParser.parseRequiredBindings(environment, iterator, declareElement, false, false);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
		}

		List<OptionalParameter> optionalBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_OPTIONAL.eq(currentElement)) {
			final OptionalParseResult optionalParseResult
					= LambdaListParser.parseOptionalBindings(environment, iterator, declareElement, false, false);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
		}

		RestParameter restBinding = null;
		if (CommonLispSymbols.AND_REST.eq(currentElement)) {
			final RestParseResult restParseResult
					= LambdaListParser.parseRestBinding(environment, iterator, declareElement, false);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
		}

		boolean keyNotProvided = true;

		List<KeyParameter> keyBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_KEY.eq(currentElement)) {
			final KeyParseResult keyParseResult
					= LambdaListParser.parseKeyBindings(environment, iterator, declareElement, false);

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

		List<AuxParameter> auxBindings = Collections.emptyList();
		if (CommonLispSymbols.AND_AUX.eq(currentElement)) {
			final AuxParseResult auxParseResult
					= LambdaListParser.parseAuxBindings(environment, iterator, declareElement, false);

			auxBindings = auxParseResult.getAuxBindings();
		}

		if (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			throw new ProgramErrorException("Unexpected element at the end of Ordinary Lambda List: " + element);
		}

		return new OrdinaryLambdaList(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}
}
