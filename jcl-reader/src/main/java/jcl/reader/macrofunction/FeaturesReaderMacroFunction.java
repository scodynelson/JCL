/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.util.Iterator;

import jcl.lang.BooleanStruct;
import jcl.lang.CommonLispSymbols;
import jcl.lang.CompilerVariables;
import jcl.lang.ConsStruct;
import jcl.lang.GlobalPackageStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageVariables;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Reader Macro Function for handling the reading of *features* in the system, handling whether or not those specific
 * features should be hidden or not (aka. the token is read in but ignored).
 */
@Component
final class FeaturesReaderMacroFunction {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesReaderMacroFunction.class);

	/**
	 * Reads in the next set of *features*, following the {@code shouldHideFeatures} property to properly suppress the
	 * read operation or not.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the next token
	 * @param shouldHideFeatures
	 * 		whether or not the *features* read should be hidden or not (aka. the token is read in but ignored)
	 */
	void readFeatures(final Reader reader, final boolean shouldHideFeatures) {

		final BooleanStruct previousReadSuppress = ReaderVariables.READ_SUPPRESS.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();
		try {
			ReaderVariables.READ_SUPPRESS.setValue(NILStruct.INSTANCE);

			PackageVariables.PACKAGE.setValue(GlobalPackageStruct.KEYWORD);
			final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
			PackageVariables.PACKAGE.setValue(previousPackage);

			final boolean isFeature = isFeature(token);
			if (isFeature && shouldHideFeatures) {
				ReaderVariables.READ_SUPPRESS.setValue(TStruct.INSTANCE);
				reader.read(true, NILStruct.INSTANCE, true);
			}
		} catch (final ReaderErrorException ree) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("Error occurred when reading feature.", ree);
			}
		} finally {
			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READ_SUPPRESS.setValue(previousReadSuppress);
		}
	}

	/**
	 * Determines if the provided {@link LispStruct} is a feature that should be read in or not.
	 *
	 * @param token
	 * 		the {@link LispStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link LispStruct} is a feature that should be read in; false otherwise
	 */
	private boolean isFeature(final LispStruct token) {
		if (token instanceof ListStruct) {
			return isListFeature((ListStruct) token);
		} else {
			return CompilerVariables.FEATURES.getVariableValue().stream().anyMatch(token::equals);
		}
	}

	/**
	 * Determines if the provided {@link ListStruct} is a feature that should be read in or not. If it is not an
	 * instance of {@link ConsStruct}, it is automatically not a feature.
	 *
	 * @param listToken
	 * 		the {@link ListStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ListStruct} is a feature that should be read in; false otherwise
	 */
	private boolean isListFeature(final ListStruct listToken) {
		return (listToken instanceof ConsStruct) && isConsFeature((ConsStruct) listToken);
	}

	/**
	 * Determines if the provided {@link ConsStruct} is a feature that should be read in or not.
	 *
	 * @param consToken
	 * 		the {@link ConsStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ConsStruct} is a feature that should be read in; false otherwise
	 */
	private boolean isConsFeature(final ConsStruct consToken) {
		final Iterator<LispStruct> iterator = consToken.iterator();
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final SymbolStruct featureOperator = (SymbolStruct) first;
		if (featureOperator.equals(CommonLispSymbols.NOT_KEYWORD)) {
			final LispStruct firstOfRest = iterator.next();
			return !isFeature(firstOfRest);
		} else if (featureOperator.equals(CommonLispSymbols.AND_KEYWORD)) {
			return isAndConsFeature(iterator);
		} else if (featureOperator.equals(CommonLispSymbols.OR_KEYWORD)) {
			return isOrConsFeature(iterator);
		} else {
			throw new ReaderErrorException("Unknown operator in feature expression: " + featureOperator);
		}
	}

	/**
	 * Determines if all of the elements are features.
	 *
	 * @param iterator
	 * 		the an {@link Iterator} for elements to check are features
	 *
	 * @return true if all of the elements are features; false otherwise
	 */
	private boolean isAndConsFeature(final Iterator<LispStruct> iterator) {

		boolean isFeature = true;
		while (iterator.hasNext()) {
			final LispStruct token = iterator.next();
			isFeature = isFeature && isFeature(token);
		}
		return isFeature;
	}

	/**
	 * Determines if any of the elements are features.
	 *
	 * @param iterator
	 * 		the an {@link Iterator} for elements to check are features
	 *
	 * @return true if any of the elements are features; false otherwise
	 */
	private boolean isOrConsFeature(final Iterator<LispStruct> iterator) {

		boolean isFeature = false;
		while (iterator.hasNext()) {
			final LispStruct token = iterator.next();
			isFeature = isFeature || isFeature(token);
		}
		return isFeature;
	}
}
