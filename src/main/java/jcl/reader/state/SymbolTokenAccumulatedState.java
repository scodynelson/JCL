package jcl.reader.state;

import jcl.syntax.AttributeType;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.syntax.reader.TokenAttribute;
import jcl.variables.PackageVariable;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;
import java.util.Map;

/**
 * Step 10.2 of the Reader Algorithm.
 * <p/>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Symbol
 * 2) Package with a Symbol
 * <p/>
 * First we attempt to see if it contains any Package Markers, if it does, then
 * we attempt to get the package for it based on 3 formats.  The formats are as follows:
 * <p/>
 * 1) ":SYMBOL_NAME" - This format should find the symbol in the Keyword Package.
 * <p/>
 * 2) "PACKAGE_NAME:SYMBOL_NAME" - This format will have to find the package and then find the
 * symbol that is external to that package.
 * <p/>
 * 3) "PACKAGE_NAME::SYMBOL_NAME" - This format will have to find the package and then intern the
 * symbol that is internal to that package.
 * <p/>
 * 4) Any other combinations of Package Markers will result in an error.
 * <p/>
 * After the token has been made into an object, that object is set as the return object for the read
 * function.  We then return the EndState.
 * <p/>
 * This will have to be fixed because it only handles Symbols using the Common Lisp Package
 * and Symbols that are in the Keyword Package!!!
 * <p/>
 */
public class SymbolTokenAccumulatedState implements State {

	public static final State SYMBOL_TOKEN_ACCUMULATED_STATE = new SymbolTokenAccumulatedState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return EndState       the final accepting state
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final SymbolStruct<?> symbolToken = getSymbolToken(readerState);
//		if (symbolToken == null) {
//			readerState.setNextState(ErrorState.ERROR_STATE);
//		} else {
		readerState.setReturnToken(symbolToken);
		readerState.setNextState(EndState.END_STATE);
//		}
		return readerState;
	}

	/**
	 * This method gets a symbolToken from the provided readerState and it's tokenAttributes.
	 *
	 * @param readerState the reader state containing the tokenAttributes to derive the symbolToken
	 * @return the built symbolToken value
	 */
	private static SymbolStruct<?> getSymbolToken(final ReaderState readerState) {

		final LinkedList<TokenAttribute> tokenAttributes = readerState.getTokenAttributes();

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoPackageMarkers = StateUtils.hasNoAttribute(tokenAttributes, AttributeType.PACKAGEMARKER);
		if (hasNoPackageMarkers) {
			final String symName = StateUtils.convertTokensToString(tokenAttributes);

			final PackageStruct pkg = PackageVariable.INSTANCE.getValue();
			final PackageSymbolStruct packageSymbol = pkg.findSymbol(symName);
			if (packageSymbol == null) {
//				readerState.setErrorMessage("Unbound variable: " + symName); // TODO: This check will happen in the compiler...
				return new SymbolStruct(symName);
			}
			return packageSymbol.getSymbolStruct();
		}

		// Check if last element is a 'PACKAGEMARKER'
		final TokenAttribute lastToken = tokenAttributes.getLast();
		if (lastToken.getAttributeType() == AttributeType.PACKAGEMARKER) {
			readerState.setErrorMessage("Illegal symbol syntax.");
			return null;
		}

		// Grab tokens up to the first 'PACKAGEMARKER' (this is the package name)
		final LinkedList<TokenAttribute> packageTokenAttributes = new LinkedList<>();

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
				symbolStartIndex = i;
				break;
			}

			if (packageMarkerCount > 2) {
				symbolStartIndex = i;
				break;
			}
		}

		// Grab the rest of the tokens (this is the symbol name)
		final LinkedList<TokenAttribute> symbolTokenAttributes = new LinkedList<>();

		for (int i = symbolStartIndex; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			symbolTokenAttributes.add(tokenAttribute);
		}

		final String pkgName = StateUtils.convertTokensToString(packageTokenAttributes);
		final String symName = StateUtils.convertTokensToString(symbolTokenAttributes);

		if (StringUtils.isNotEmpty(pkgName)) {
			final PackageStruct pkg = PackageStruct.findPackage(pkgName);

			if (pkg == null) {
				readerState.setErrorMessage("There is no package named " + pkgName);
				return null;
			}

			if (packageMarkerCount == 1) {
				final Map<String, SymbolStruct<?>> pkgExternalSymbols = pkg.getExternalSymbols();

				final SymbolStruct<?> externalSymbol = pkgExternalSymbols.get(symName);
				if (externalSymbol == null) {
					readerState.setErrorMessage("No external symbol named \"" + symName + "\" in package " + pkgName);
					return null;
				}
				return externalSymbol;
			} else {
				final PackageSymbolStruct packageSymbol = pkg.findSymbol(symName);

				final SymbolStruct<?> symbol = packageSymbol.getSymbolStruct();
				if (symbol == null) {
					readerState.setErrorMessage("Unbound variable: " + pkgName + "::" + symName);
					return null;
				}
				return symbol;
			}
		} else {
			final PackageSymbolStruct pkgSymStruct = GlobalPackageStruct.KEYWORD.findSymbol(symName);
			if (pkgSymStruct == null) {
				return new KeywordSymbolStruct(symName);
			} else {
				return pkgSymStruct.getSymbolStruct();
			}
		}
	}
}
