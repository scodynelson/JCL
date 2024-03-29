/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import jcl.lang.AttributeType;
import jcl.lang.KeywordStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import lombok.experimental.UtilityClass;

/**
 * Step 10.2 of the Reader Algorithm.
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * <ol>
 *     <li>Symbol</li>
 *     <li>Package with a Symbol</li>
 * </ol>
 * <p>
 * First we attempt to see if it contains any Package Markers, if it does, then we attempt to get the package for it
 * based on 3 formats.  The formats are as follows:
 * </p>
 * <p>
 * 1) ":SYMBOL_NAME" - This format should find the symbol in the Keyword Package.
 * </p>
 * <p>
 * 2) "PACKAGE_NAME:SYMBOL_NAME" - This format will have to find the package and then find the symbol that is external
 * to that package.
 * </p>
 * <p>
 * 3) "PACKAGE_NAME::SYMBOL_NAME" - This format will have to find the package and then intern the symbol that is
 * internal to that package.
 * </p>
 * <p>
 * 4) Any other combinations of Package Markers will result in an error.
 * </p>
 * <p>
 * After the token has been made into an object, that object is set as the return object for the read function.  We then
 * return the EndState.
 * </p>
 */
@UtilityClass
final class SymbolTokenAccumulatedReaderState {

	/**
	 * This method gets a {@link SymbolStruct} from the provided {@link TokenBuilder} and it's list of
	 * {@link TokenAttribute} objects.
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the list of {@link TokenAttribute} objects to derive the {@link SymbolStruct}
	 *
	 * @return the built {@link SymbolStruct} value
	 */
	static SymbolStruct getSymbolToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoPackageMarkers = ReaderProcessor.hasNoAttributesWithAttributeType(
				tokenAttributes, AttributeType.PACKAGEMARKER
		);
		if (hasNoPackageMarkers) {
			final String symbolName = ReaderProcessor.convertTokenAttributesToString(tokenAttributes);

			final PackageStruct symbolPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();
			return findExistingOrCreateNewSymbol(symbolName, symbolPackage);
		}

		// Check if last element is a 'PACKAGEMARKER'
		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		if (lastTokenAttribute.getAttributeType() == AttributeType.PACKAGEMARKER) {
			throw new ReaderErrorException("Illegal symbol syntax.");
		}

		// Grab tokens up to the first 'PACKAGEMARKER' (this is the package name)
		final List<TokenAttribute> packageNameTokenAttributes = new ArrayList<>();

		int packageMarkerStartIndex = 0;
		for (int i = 0; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			final AttributeType attributeType = tokenAttribute.getAttributeType();

			if (attributeType == AttributeType.PACKAGEMARKER) {
				packageMarkerStartIndex = i;
				break;
			}

			packageNameTokenAttributes.add(tokenAttribute);
		}

		// Skip up to 2 'PACKAGEMARKER' tokens or until a different attribute is found
		int packageMarkerCount = 0;
		int symbolStartIndex = 0;
		for (int i = packageMarkerStartIndex; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			final AttributeType attributeType = tokenAttribute.getAttributeType();

			if (attributeType == AttributeType.PACKAGEMARKER) {
				packageMarkerCount++;
			} else {
				// Non package marker found; break
				symbolStartIndex = i;
				break;
			}

			// NOTE: we have this here and not somehow in the above 'if' because we need to use the results of packageMarkerCount
			final int maxNumberOfPackageMarkers = 2;
			if (packageMarkerCount > maxNumberOfPackageMarkers) {
				// Max number of package markers hit. All the rest are part of the symbol.
				symbolStartIndex = i;
				break;
			}
		}

		// Grab the rest of the tokens (this is the symbol name)
		final List<TokenAttribute> symbolNameTokenAttributes = new ArrayList<>();

		for (int i = symbolStartIndex; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			symbolNameTokenAttributes.add(tokenAttribute);
		}

		final String packageName = ReaderProcessor.convertTokenAttributesToString(packageNameTokenAttributes);
		final String symbolName = ReaderProcessor.convertTokenAttributesToString(symbolNameTokenAttributes);

		if (!packageName.isEmpty()) {
			final PackageStruct symbolPackage = PackageStruct.findPackage(packageName);

			if (packageMarkerCount == 1) {
				final Optional<SymbolStruct> externalSymbol = symbolPackage.findExternalSymbol(symbolName);
				if (externalSymbol.isEmpty()) {
					final String message = "No external symbol named \"" + symbolName + "\" in package " + packageName;
					throw new ReaderErrorException(message);
				}
				return externalSymbol.get();
			} else {
				return findExistingOrCreateNewSymbol(symbolName, symbolPackage);
			}
		}

		final PackageStruct symbolPackage = GlobalPackageStruct.KEYWORD;
		return findExistingOrCreateNewSymbol(symbolName, symbolPackage);
	}

	/**
	 * Either finds the existing {@link SymbolStruct} within the provided {@link PackageStruct} using
	 * {@link PackageStruct#findSymbol} or creates a new {@link SymbolStruct}. If the provided {@code PackageStruct} is
	 * equal to {@link GlobalPackageStruct#KEYWORD}, a {@link KeywordStruct} will be returned instead.
	 *
	 * @param symbolName
	 * 		the name of the {@link SymbolStruct} to find or create
	 * @param symbolPackage
	 * 		the {@link PackageStruct} to either find the {@link SymbolStruct} or create and intern
	 *
	 * @return the existing {@link SymbolStruct} or a newly created one
	 */
	private static SymbolStruct findExistingOrCreateNewSymbol(final String symbolName, final PackageStruct symbolPackage) {

		final PackageSymbolStruct foundSymbol = symbolPackage.findSymbol(symbolName);
		if (foundSymbol.notFound()) {
			final boolean isKeyword = GlobalPackageStruct.KEYWORD.eq(symbolPackage);
			if (isKeyword) {
				return KeywordStruct.toLispKeyword(symbolName);
			}
			return symbolPackage.intern(symbolName).getSymbol();
		} else {
			return foundSymbol.getSymbol();
		}
	}
}
