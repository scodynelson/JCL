/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.packages.PackageVariables;
import jcl.reader.AttributeType;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.symbols.SymbolStruct;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Step 10.2 of the Reader Algorithm.
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Symbol
 * 2) Package with a Symbol
 * </p>
 * <p>
 * First we attempt to see if it contains any Package Markers, if it does, then we attempt to get the package for it
 * based on 3 formats.  The formats are as follows:
 * </p>
 * <tab>
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
 * </tab>
 * <p>
 * After the token has been made into an object, that object is set as the return object for the read function.  We
 * then
 * return the EndState.
 * </p>
 * <p>
 * This will have to be fixed because it only handles Symbols using the Common Lisp Package and Symbols that are in the
 * Keyword Package!!!
 * </p>
 */
@Component
class SymbolTokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4237075506539486959L;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public SimpleElement process(final TokenBuilder tokenBuilder) {

		final SymbolElement symbolElement = getSymbolElement(tokenBuilder);
		if (symbolElement == null) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		} else {
			return symbolElement;
		}
	}

	/**
	 * This method gets a {@link SymbolElement} from the provided {@link TokenBuilder} and it's {@link
	 * TokenBuilder#tokenAttributes}.
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the {@link TokenBuilder#tokenAttributes} to derive the {@link SymbolElement}
	 *
	 * @return the built {@link SymbolElement} value
	 */
	private static SymbolElement getSymbolElement(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// If there are no tokens, not a symbol. NOTE: We should never get here in the sequence. This is a protection.
		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return null;
		}

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoPackageMarkers = ReaderState.hasNoAttributes(tokenAttributes, AttributeType.PACKAGEMARKER);
		if (hasNoPackageMarkers) {
			final String symName = ReaderState.convertTokensToString(tokenAttributes);

			final PackageStruct pkg = PackageVariables.PACKAGE.getValue();

			final PackageSymbolStruct foundSymbol = pkg.findSymbol(symName);
			if (foundSymbol == null) {
				return new SymbolElement(pkg.getName(), symName);
//			    throw new ReaderErrorException("Unbound variable: " + symName); // TODO: This check will happen in the compiler...
			} else {
				final SymbolStruct<?> foundSymbolStruct = foundSymbol.getSymbolStruct();
				return new SymbolElement(foundSymbolStruct.getSymbolPackage().getName(), foundSymbolStruct.getName());
			}
		}

		// Check if last element is a 'PACKAGEMARKER'
		final TokenAttribute lastToken = tokenAttributes.getLast();
		if (lastToken.getAttributeType() == AttributeType.PACKAGEMARKER) {
			throw new ReaderErrorException("Illegal symbol syntax.");
		}

		// Grab tokens up to the first 'PACKAGEMARKER' (this is the package name)
		final List<TokenAttribute> packageTokenAttributes = new ArrayList<>();

		int packageMarkerStartIndex = 0;
		for (int i = 0; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			final AttributeType attributeType = tokenAttribute.getAttributeType();

			if (attributeType == AttributeType.PACKAGEMARKER) {
				packageMarkerStartIndex = i;
				break;
			}

			packageTokenAttributes.add(tokenAttribute);
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
		final List<TokenAttribute> symbolTokenAttributes = new ArrayList<>();

		for (int i = symbolStartIndex; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			symbolTokenAttributes.add(tokenAttribute);
		}

		final String pkgName = ReaderState.convertTokensToString(packageTokenAttributes);
		final String symName = ReaderState.convertTokensToString(symbolTokenAttributes);

		if (StringUtils.isNotEmpty(pkgName)) {
			final PackageStruct pkg = PackageStruct.findPackage(pkgName);

			if (pkg == null) {
				throw new ReaderErrorException("There is no package named " + pkgName);
			}

			final int singlePackageMarker = 1;
			if (packageMarkerCount == singlePackageMarker) {
				final Map<String, SymbolStruct<?>> pkgExternalSymbols = pkg.getExternalSymbols();

				final SymbolStruct<?> externalSymbol = pkgExternalSymbols.get(symName);
				if (externalSymbol == null) {
					throw new ReaderErrorException("No external symbol named \"" + symName + "\" in package " + pkgName);
				}
				return new SymbolElement(pkgName, symName, true);
//				throw new ReaderErrorException("Unbound variable: " + symName); // TODO: This check will happen in the compiler...
			} else {
				return new SymbolElement(pkgName, symName);
//				throw new ReaderErrorException("Unbound variable: " + pkgName + "::" + symName); // TODO: This check will happen in the compiler...
			}
		}

		final PackageStruct pkg = GlobalPackageStruct.KEYWORD;
		return new SymbolElement(pkg.getName(), symName);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
