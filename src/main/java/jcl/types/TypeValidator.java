/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.util.Arrays;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.LispType;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TypeValidator {

	@Autowired
	private Printer printer;

	@SuppressWarnings({"unchecked", "SuppressionAnnotation"})
	public <T> T validateType(final LispStruct object, final String analyzerName, final String objectName,
	                          final LispType type, final Class<T> classType) {
		validateTypes(object, analyzerName, objectName, type);
		return (T) object;
	}

	public void validateTypes(final LispStruct object, final String functionName, final String objectName,
	                          final LispType... types) {

		final LispType type = object.getType();
		final boolean noneMatch = Stream.of(types)
		                                .noneMatch(e -> e.equals(type));

		if (noneMatch) {
			final String printedObject = printer.print(object);
			throw new TypeErrorException(functionName + ": " + objectName + " must be one of the following types: " + Arrays.toString(types) + ". Got: " + printedObject);
		}
	}
}
