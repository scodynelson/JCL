/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.util.Iterator;

import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.reader.Reader;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;

/**
 * Reader Macro Function for handling the reading of *features* in the system, handling whether or not those specific
 * features should be hidden or not (aka. the token is read in but ignored).
 */
@Log4j2
@UtilityClass
final class FeaturesReaderMacroFunction {

	/**
	 * Reads in the next set of *features*, following the {@code shouldHideFeatures} property to properly suppress the
	 * read operation or not.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in the next token
	 * @param shouldHideFeatures
	 * 		whether or not the *features* read should be hidden or not (aka. the token is read in but ignored)
	 */
	static void readFeatures(final InputStreamStruct inputStreamStruct, final boolean shouldHideFeatures) {

		final BooleanStruct previousReadSuppress = CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue();
		final PackageStruct previousPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();
		try {
			CommonLispSymbols.READ_SUPPRESS_VAR.setSymbolValue(NILStruct.INSTANCE);

			CommonLispSymbols.PACKAGE_VAR.setSymbolValue(GlobalPackageStruct.KEYWORD);
			final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			CommonLispSymbols.PACKAGE_VAR.setSymbolValue(previousPackage);

			final boolean isFeature = isFeature(token);
			if (isFeature && shouldHideFeatures) {
				CommonLispSymbols.READ_SUPPRESS_VAR.setSymbolValue(TStruct.INSTANCE);
				Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			}
		} catch (final ReaderErrorException ree) {
			if (log.isDebugEnabled()) {
				log.debug("Error occurred when reading feature.", ree);
			}
		} finally {
			CommonLispSymbols.PACKAGE_VAR.setSymbolValue(previousPackage);
			CommonLispSymbols.READ_SUPPRESS_VAR.setSymbolValue(previousReadSuppress);
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
	private static boolean isFeature(final LispStruct token) {
		if (token instanceof ListStruct) {
			return isListFeature((ListStruct) token);
		} else {
			return CommonLispSymbols.FEATURES_VAR.getVariableValue().stream().anyMatch(token::equals);
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
	private static boolean isListFeature(final ListStruct listToken) {
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
	private static boolean isConsFeature(final ConsStruct consToken) {
		final Iterator<LispStruct> iterator = consToken.iterator();
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final SymbolStruct featureOperator = (SymbolStruct) first;
		if (featureOperator.eq(CommonLispSymbols.NOT_KEYWORD)) {
			final LispStruct firstOfRest = iterator.next();
			return !isFeature(firstOfRest);
		} else if (featureOperator.eq(CommonLispSymbols.AND_KEYWORD)) {
			return isAndConsFeature(iterator);
		} else if (featureOperator.eq(CommonLispSymbols.OR_KEYWORD)) {
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
	private static boolean isAndConsFeature(final Iterator<LispStruct> iterator) {

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
	private static boolean isOrConsFeature(final Iterator<LispStruct> iterator) {

		boolean isFeature = false;
		while (iterator.hasNext()) {
			final LispStruct token = iterator.next();
			isFeature = isFeature || isFeature(token);
		}
		return isFeature;
	}
}
