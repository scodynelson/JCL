package jcl.reader;

import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.TokenAttribute;
import jcl.reader.syntax.TokenBuilder;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.packages.PackageSymbolStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;
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
public class SymbolTokenAccumulatedState extends State {

	public static final State SYMBOL_TOKEN_ACCUMULATED_STATE = new SymbolTokenAccumulatedState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final SymbolStruct<?> symbolToken = getSymbolToken(tokenBuilder);
		if (symbolToken == null) {
			final ErrorState errorState = new ErrorState(this);
			errorState.process(reader, tokenBuilder);
		} else {
			tokenBuilder.setReturnToken(symbolToken);
		}
	}

	/**
	 * This method gets a symbolToken from the provided tokenBuilder and it's tokenAttributes.
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the tokenAttributes to derive the symbolToken
	 *
	 * @return the built symbolToken value
	 */
	private static SymbolStruct<?> getSymbolToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoPackageMarkers = hasNoAttributes(tokenAttributes, AttributeType.PACKAGEMARKER);
		if (hasNoPackageMarkers) {
			final String symName = convertTokensToString(tokenAttributes);

			final PackageStruct pkg = Variable.PACKAGE.getValue();
			final PackageSymbolStruct packageSymbol = pkg.findSymbol(symName);
			if (packageSymbol == null) {
//				tokenBuilder.setErrorMessage("Unbound variable: " + symName); // TODO: This check will happen in the compiler...
				return new SymbolStruct<>(symName, pkg);
			}
			return packageSymbol.getSymbolStruct();
		}

		// Check if last element is a 'PACKAGEMARKER'
		final TokenAttribute lastToken = tokenAttributes.getLast();
		if (lastToken.getAttributeType() == AttributeType.PACKAGEMARKER) {
			throw new ReaderErrorException("Illegal symbol syntax.");
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

		final String pkgName = convertTokensToString(packageTokenAttributes);
		final String symName = convertTokensToString(symbolTokenAttributes);

		if (StringUtils.isNotEmpty(pkgName)) {
			final PackageStruct pkg = PackageStruct.findPackage(pkgName);

			if (pkg == null) {
				throw new ReaderErrorException("There is no package named " + pkgName);
			}

			if (packageMarkerCount == 1) {
				final Map<String, SymbolStruct<?>> pkgExternalSymbols = pkg.getExternalSymbols();

				final SymbolStruct<?> externalSymbol = pkgExternalSymbols.get(symName);
				if (externalSymbol == null) {
					throw new ReaderErrorException("No external symbol named \"" + symName + "\" in package " + pkgName);
				}
				return externalSymbol;
			} else {
				final PackageSymbolStruct packageSymbol = pkg.findSymbol(symName);

				final SymbolStruct<?> symbol = packageSymbol.getSymbolStruct();
				if (symbol == null) {
					throw new ReaderErrorException("Unbound variable: " + pkgName + "::" + symName);
				}
				return symbol;
			}
		} else {
			final PackageSymbolStruct pkgSymStruct = GlobalPackageStruct.KEYWORD.findSymbol(symName);
			return (pkgSymStruct == null) ? new KeywordSymbolStruct(symName) : pkgSymStruct.getSymbolStruct();
		}
	}
}
