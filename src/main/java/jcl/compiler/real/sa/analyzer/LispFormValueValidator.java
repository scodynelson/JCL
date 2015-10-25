/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.Arrays;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LispFormValueValidator {

	@Autowired
	private Printer printer;

	public int validateListFormSizeEven(final ListStruct form, final String analyzerName) {
		final int formSize = form.size();
		if ((formSize % 2) != 0) {
			throw new ProgramErrorException(analyzerName + ": Odd number of arguments received: " + formSize + ". Expected an even number of arguments.");
		}
		return formSize;
	}

	public int validateListFormSizeExact(final ListStruct form, final int amount, final String analyzerName) {
		final int formSize = form.size();
		if (formSize != amount) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected " + amount + " arguments.");
		}
		return formSize;
	}

	public int validateListFormSize(final ListStruct form, final int lowerBound, final String analyzerName) {
		final int formSize = form.size();
		if (formSize < lowerBound) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected at least " + lowerBound + " arguments.");
		}
		return formSize;
	}

	public int validateListFormSize(final ListStruct form, final int lowerBound, final int upperBound, final String analyzerName) {
		final int formSize = form.size();
		if ((formSize < lowerBound) || (formSize > upperBound)) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected between " + lowerBound + " and " + upperBound + " arguments.");
		}
		return formSize;
	}

	public int validateListParameterSize(final ListStruct form, final int lowerBound, final int upperBound, final String analyzerName) {
		final int formSize = form.size();
		if ((formSize < lowerBound) || (formSize > upperBound)) {
			throw new ProgramErrorException(analyzerName + ": List parameter must have between " + lowerBound + " and " + upperBound + " elements. Got: " + formSize);
		}
		return formSize;
	}

	@SuppressWarnings({"unchecked", "SuppressionAnnotation"})
	public <T> T validateObjectType(final LispStruct object, final String analyzerName, final String objectName,
	                                final Class<T> classType) {
		validateObjectTypes(object, analyzerName, objectName, classType);
		return (T) object;
	}

	public void validateObjectTypes(final LispStruct object, final String analyzerName, final String objectName,
	                                final Class<?>... classTypes) {

		final Class<?> objectClass = object.getClass();
		final boolean noneMatch = Stream.of(classTypes)
		                                .noneMatch(e -> e.isAssignableFrom(objectClass));

		if (noneMatch) {
			final String printedObject = printer.print(object);
			throw new ProgramErrorException(analyzerName + ": " + objectName + " must be of of the following: " + Arrays.toString(classTypes) + ". Got: " + printedObject);
		}
	}

	public SymbolStruct<?> validateSymbolOrNIL(final LispStruct object, final String analyzerName,
	                                           final String objectName) {
		if (NILStruct.INSTANCE.equals(object) || NullStruct.INSTANCE.equals(object)) {
			return null;
		} else if (object instanceof SymbolStruct) {
			return (SymbolStruct<?>) object;
		} else {
			final String printedObject = printer.print(object);
			throw new ProgramErrorException(analyzerName + ": " + objectName + " must be a Symbol or NIL. Got: " + printedObject);
		}
	}
}
