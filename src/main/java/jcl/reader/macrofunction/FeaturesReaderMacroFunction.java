/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
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

import java.util.ArrayList;
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
	private static final SymbolElement NOT = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "NOT");

	/**
	 * And {@link KeywordSymbolStruct} for processing features that should be included via 'and' operation.
	 */
	private static final SymbolElement AND = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "AND");

	/**
	 * Or {@link KeywordSymbolStruct} for processing features that should be included via 'or' operation.
	 */
	private static final SymbolElement OR = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "OR");

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
		// TODO: i need to revisit this logic at some point...

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
		if (lispStruct instanceof ListElement) {
			return isListFeature((ListElement) lispStruct);
		} else {
			final List<LispStruct> featuresList = CompilerVariables.FEATURES.getValue().getAsJavaList();
			return featuresList.contains(lispStruct.toLispStruct()); // TODO: can we fix this??
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
	private static boolean isListFeature(final ListElement listStruct) {
		return (listStruct instanceof ConsElement) && isConsFeature((ConsElement) listStruct);
	}

	/**
	 * Determines if the provided {@link ConsStruct} is a feature that should be read in or not.
	 *
	 * @param consStruct
	 * 		the {@link ConsStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ConsStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isConsFeature(final ConsElement consStruct) {
		final List<SimpleElement> elements = consStruct.getElements();
		final SimpleElement first = elements.get(0);

		if (!(first instanceof SymbolElement)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		// TODO: we should replace the ConsStruct elements list with a Customized LinkedList that allows us to get the "rest" of elements
		final List<SimpleElement> rest = new ArrayList<>(elements);
		rest.remove(0);

		final SymbolElement featureOperator = (SymbolElement) first;
		if (featureOperator.equals(NOT)) {
			return !isFeature(rest.get(0));
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
	private static boolean isAndConsFeature(final List<SimpleElement> elements) {
		boolean tempReturnVal = true;
		for (final SimpleElement lispStruct : elements) {
			tempReturnVal = tempReturnVal && isFeature(lispStruct);
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
	private static boolean isOrConsFeature(final List<SimpleElement> elements) {
		boolean tempReturnVal = false;
		for (final SimpleElement lispStruct : elements) {
			tempReturnVal = tempReturnVal || isFeature(lispStruct);
		}
		return tempReturnVal;
	}
}
