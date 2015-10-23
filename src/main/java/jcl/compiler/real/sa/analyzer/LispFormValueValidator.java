/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LispFormValueValidator {

	@Autowired
	private Printer printer;

	public void validateListFormSizeEven(final ListStruct form, final String analyzerName) {
		final int formSize = form.size();
		if ((formSize % 2) != 0) {
			throw new ProgramErrorException(analyzerName + ": Odd number of arguments received: " + formSize + ". Expected an even number of arguments.");
		}
	}

	public void validateListFormSizeExact(final ListStruct form, final int amount, final String analyzerName) {
		final int formSize = form.size();
		if (formSize != amount) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected " + amount + " arguments.");
		}
	}

	public void validateListFormSize(final ListStruct form, final int lowerBound, final String analyzerName) {
		final int formSize = form.size();
		if (formSize < lowerBound) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected at least " + lowerBound + " arguments.");
		}
	}

	public void validateListFormSize(final ListStruct form, final int lowerBound, final int upperBound, final String analyzerName) {
		final int formSize = form.size();
		if ((formSize < lowerBound) || (formSize > upperBound)) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + formSize + ". Expected between " + lowerBound + " and " + upperBound + " arguments.");
		}
	}

	@SuppressWarnings({"unchecked", "SuppressionAnnotation"})
	public <T> T validateObjectType(final LispStruct object, final Class<T> classType, final String analyzerName,
	                                final String objectName) {
		if (!object.getClass().isAssignableFrom(classType)) {
			final String printedObject = printer.print(object);
			throw new ProgramErrorException(analyzerName + ": " + objectName + " must be a " + classType.getSimpleName() + ". Got: " + printedObject);
		}
		return (T) object;
	}
}
