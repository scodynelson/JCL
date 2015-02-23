/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Reader Macro Function for handling the reading of *features* in the system, handling whether or not those specific
 * features should be hidden or not (aka. the token is read in but ignored).
 */
final class FeaturesReaderMacroFunction {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesReaderMacroFunction.class);

	// TODO: is this where we keep these next 3???

	/**
	 * Not {@link KeywordSymbolStruct} for processing features that should 'not' be included.
	 */
	private static final KeywordSymbolStruct NOT = new KeywordSymbolStruct("NOT");

	/**
	 * And {@link KeywordSymbolStruct} for processing features that should be included via 'and' operation.
	 */
	private static final KeywordSymbolStruct AND = new KeywordSymbolStruct("AND");

	/**
	 * Or {@link KeywordSymbolStruct} for processing features that should be included via 'or' operation.
	 */
	private static final KeywordSymbolStruct OR = new KeywordSymbolStruct("OR");

	/**
	 * Private constructor.
	 */
	private FeaturesReaderMacroFunction() {
	}

	/**
	 * Reads in the next set of *features*, following the {@code shouldHideFeatures} property to properly suppress the
	 * read operation or not.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the next token
	 * @param shouldHideFeatures
	 * 		whether or not the *features* read should be hidden or not (aka. the token is read in but ignored)
	 */
	static void readFeatures(final Reader reader, final boolean shouldHideFeatures) {
		final BooleanStruct previousReadSuppress = ReaderVariables.READ_SUPPRESS.getValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getValue();
		try {
			ReaderVariables.READ_SUPPRESS.setValue(NILStruct.INSTANCE);

			PackageVariables.PACKAGE.setValue(GlobalPackageStruct.KEYWORD);
			final SimpleElement lispStruct = reader.read();
			PackageVariables.PACKAGE.setValue(previousPackage);

			final boolean isFeature = isFeature(lispStruct);
			if (isFeature && shouldHideFeatures) {
				ReaderVariables.READ_SUPPRESS.setValue(TStruct.INSTANCE);
				reader.read();
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
	 * @param lispStruct
	 * 		the {@link LispStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link LispStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isFeature(final SimpleElement lispStruct) {
		if (lispStruct instanceof ConsElement) {
			return isListFeature((ConsElement) lispStruct);
		} else {
			final List<LispStruct> featuresList = CompilerVariables.FEATURES.getValue().getAsJavaList();
			return featuresList.contains(lispStruct.toLispStruct());
		}
	}

	/**
	 * Determines if the provided {@link ListStruct} is a feature that should be read in or not. If it is not an
	 * instance of {@link ConsStruct}, it is automatically not a feature.
	 *
	 * @param listStruct
	 * 		the {@link ListStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ListStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isListFeature(final ConsElement listStruct) {
		return (listStruct.toLispStruct() instanceof ConsStruct) && isConsFeature(listStruct); // TODO: fix
	}

	/**
	 * Determines if the provided {@link ConsStruct} is a feature that should be read in or not.
	 *
	 * @param consStruct
	 * 		the {@link ConsStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ConsStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isConsFeature(final ConsElement consStruct) { // TODO: fix
		final LispStruct first = ((ConsStruct) consStruct.toLispStruct()).getFirst();

		if (!(first instanceof KeywordSymbolStruct)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final List<LispStruct> rest = ((ConsStruct) consStruct.toLispStruct()).getRest().getAsJavaList();

		final KeywordSymbolStruct featureOperator = (KeywordSymbolStruct) first;
		if (featureOperator.equals(NOT)) {
			return !isFeature((SimpleElement) rest.get(0)); // TODO: FIX THIS!!!!
		} else if (featureOperator.equals(AND)) {
			return isAndConsFeature(rest);
		} else if (featureOperator.equals(OR)) {
			return isOrConsFeature(rest);
		} else {
			throw new ReaderErrorException("Unknown operator in feature expression: " + featureOperator);
		}
	}

	/**
	 * Determines if all of the elements are features.
	 *
	 * @param elements
	 * 		the elements to check are features
	 *
	 * @return true if all of the elements are features; false otherwise
	 */
	private static boolean isAndConsFeature(final List<LispStruct> elements) { // TODO: fix
		boolean tempReturnVal = true;
		for (final LispStruct lispStruct : elements) {
			tempReturnVal = tempReturnVal && isFeature((SimpleElement) lispStruct); // TODO: FIX THIS!!!!
		}
		return tempReturnVal;
	}

	/**
	 * Determines if any of the elements are features.
	 *
	 * @param elements
	 * 		the elements to check are features
	 *
	 * @return true if any of the elements are features; false otherwise
	 */
	private static boolean isOrConsFeature(final List<LispStruct> elements) { // TODO: fix
		boolean tempReturnVal = false;
		for (final LispStruct lispStruct : elements) {
			tempReturnVal = tempReturnVal || isFeature((SimpleElement) lispStruct); // TODO: FIX THIS!!!!
		}
		return tempReturnVal;
	}
}
