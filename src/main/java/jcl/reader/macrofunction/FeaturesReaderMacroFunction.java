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
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;

/**
 * Reader Macro Function for handling the reading of *features* in the system, handling whether or not those specific
 * features should be hidden or not (aka. the token is read in but ignored).
 */
@Component
final class FeaturesReaderMacroFunction implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3522803992369397668L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesReaderMacroFunction.class);

	// TODO: is this where we keep these next 3???

	/**
	 * NOT keyword {@link SymbolElement} for processing features that should 'not' be included.
	 */
	private static final SymbolElement NOT = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "NOT");

	/**
	 * AND keyword {@link SymbolElement} for processing features that should be included via 'and' operation.
	 */
	private static final SymbolElement AND = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "AND");

	/**
	 * OR keyword {@link SymbolElement} for processing features that should be included via 'or' operation.
	 */
	private static final SymbolElement OR = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "OR");

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

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
	private boolean isFeature(final SimpleElement lispStruct) {
		if (lispStruct instanceof ListElement) {
			return isListFeature((ListElement) lispStruct);
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
	private boolean isListFeature(final ListElement listStruct) {
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
	private boolean isConsFeature(final ConsElement consStruct) {
		final EnhancedLinkedList<SimpleElement> elements = consStruct.getElements();
		final SimpleElement first = elements.getFirst();

		if (!(first instanceof SymbolElement)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final EnhancedLinkedList<SimpleElement> rest = elements.getAllButFirst();

		final SymbolElement featureOperator = (SymbolElement) first;
		if (featureOperator.equals(NOT)) {
			return !isFeature(rest.getFirst());
		} else if (featureOperator.equals(AND)) {
			return isAndConsFeature(rest);
		} else if (featureOperator.equals(OR)) {
			return isOrConsFeature(rest);
		} else {
			throw new ReaderErrorException("Unknown operator in feature expression: " + printer.print(featureOperator));
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
	private boolean isAndConsFeature(final EnhancedLinkedList<SimpleElement> elements) {
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
	private boolean isOrConsFeature(final EnhancedLinkedList<SimpleElement> elements) {
		boolean tempReturnVal = false;
		for (final SimpleElement lispStruct : elements) {
			tempReturnVal = tempReturnVal || isFeature(lispStruct);
		}
		return tempReturnVal;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
