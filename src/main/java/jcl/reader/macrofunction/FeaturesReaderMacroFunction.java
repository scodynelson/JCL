/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
			final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
			PackageVariables.PACKAGE.setValue(previousPackage);

			final boolean isFeature = isFeature(token);
			if (isFeature && shouldHideFeatures) {
				ReaderVariables.READ_SUPPRESS.setValue(TStruct.INSTANCE);
				reader.read(true, NullStruct.INSTANCE, true);
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
			final List<LispStruct> featuresList = CompilerVariables.FEATURES.getValue().getAsJavaList();
			return featuresList.contains(token);
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
		final LispStruct first = consToken.getFirst();

		if (!(first instanceof SymbolStruct)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final ListStruct rest = consToken.getRest();

		final SymbolStruct<?> featureOperator = (SymbolStruct) first;
		if (featureOperator.equals(CommonLispSymbols.NOT_KEYWORD)) {
			final LispStruct firstOfRest = rest.getFirst();
			return !isFeature(firstOfRest);
		} else if (featureOperator.equals(CommonLispSymbols.AND_KEYWORD)) {
			return isAndConsFeature(rest);
		} else if (featureOperator.equals(CommonLispSymbols.OR_KEYWORD)) {
			return isOrConsFeature(rest);
		} else {
			throw new ReaderErrorException("Unknown operator in feature expression: " + printer.print(featureOperator));
		}
	}

	/**
	 * Determines if all of the elements are features.
	 *
	 * @param listToken
	 * 		the elements to check are features
	 *
	 * @return true if all of the elements are features; false otherwise
	 */
	private boolean isAndConsFeature(final ListStruct listToken) {
		final List<LispStruct> tokensAsJavaList = listToken.getAsJavaList();

		boolean isFeature = true;
		for (final LispStruct token : tokensAsJavaList) {
			isFeature = isFeature && isFeature(token);
		}
		return isFeature;
	}

	/**
	 * Determines if any of the elements are features.
	 *
	 * @param listToken
	 * 		the elements to check are features
	 *
	 * @return true if any of the elements are features; false otherwise
	 */
	private boolean isOrConsFeature(final ListStruct listToken) {
		final List<LispStruct> tokensAsJavaList = listToken.getAsJavaList();

		boolean isFeature = false;
		for (final LispStruct token : tokensAsJavaList) {
			isFeature = isFeature || isFeature(token);
		}
		return isFeature;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
