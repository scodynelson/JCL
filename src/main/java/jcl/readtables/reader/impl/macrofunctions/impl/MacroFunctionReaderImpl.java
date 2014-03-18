package jcl.readtables.reader.impl.macrofunctions.impl;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.ReadBaseVariable;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariable;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.impl.macrofunctions.MacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.readtables.reader.syntax.ReadExtendedToken;
import jcl.symbols.SymbolStruct;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.syntax.CharacterConstants;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;
import jcl.variables.FeaturesVariable;
import jcl.variables.ReadSuppressVariable;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class MacroFunctionReaderImpl implements MacroFunctionReader {

	private final StateReader stateReader;

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroFunctionReaderImpl.class);

	public MacroFunctionReaderImpl(final StateReader stateReader) {
		this.stateReader = stateReader;
	}

	@Override
	public LispStruct read() {
		return stateReader.read();
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return stateReader.read(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadResult readChar() {
		return stateReader.readChar();
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return stateReader.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		stateReader.unreadChar(codePoint);
	}

	//*************************//
	//** READ-EXTENDED-TOKEN **//
	//*************************//

	@Override
	public ReadExtendedToken readExtendedToken() {

		final StringBuilder stringBuilder = new StringBuilder();
		final ReadResult readResult = readInternalToken(stringBuilder);

		if (readResult.wasEOF()) {
			return new ReadExtendedToken("", false, false);
		} else {
			final InternalReadExtendedToken readExtendedToken = internalReadExtendedToken(stringBuilder, readResult, false);

			final List<Integer> escapeIndices = readExtendedToken.getEscapeIndices();
			final Integer colon = readExtendedToken.getFirstPackageDelimiter();

			final String tokenWithProperCase = getTokenWithProperCase(stateReader, stringBuilder, escapeIndices);

			return new ReadExtendedToken(tokenWithProperCase, CollectionUtils.isNotEmpty(escapeIndices), colon != null);
		}
	}

	@Override
	public String readExtendedTokenEscaped() {

		final StringBuilder stringBuilder = new StringBuilder();
		final ReadResult readResult = readInternalToken(stringBuilder);

		if (readResult.wasEOF()) {
			throw new ReaderErrorException("EOF after escape character.");
		} else {
			final InternalReadExtendedToken readExtendedToken = internalReadExtendedToken(stringBuilder, readResult, true);

			final List<Integer> escapeIndices = readExtendedToken.getEscapeIndices();

			return getTokenWithProperCase(stateReader, stringBuilder, escapeIndices);
		}
	}

	private static String getTokenWithProperCase(final StateReader stateReader, final StringBuilder token, final List<Integer> escapeIndices) {
		final CaseSpec readtableCase = stateReader.getReadtableCase();

		final StringBuilder stringBuilder = new StringBuilder(token.length());
		for (int i = 0; i < token.length(); i++) {

			final int currentToken = token.codePointAt(i);
			if (escapeIndices.contains(i)) {
				stringBuilder.appendCodePoint(currentToken);
			} else {
				switch (readtableCase) {
					case UPCASE:
						final int upperCaseToken = Character.toUpperCase(currentToken);
						stringBuilder.appendCodePoint(upperCaseToken);
						break;
					case DOWNCASE:
						final int lowerCaseToken = Character.toLowerCase(currentToken);
						stringBuilder.appendCodePoint(lowerCaseToken);
						break;
					case INVERT:
						final int invertedCaseToken;
						if (Character.isUpperCase(currentToken)) {
							invertedCaseToken = Character.toLowerCase(currentToken);
						} else {
							invertedCaseToken = Character.toUpperCase(currentToken);
						}
						stringBuilder.appendCodePoint(invertedCaseToken);
						break;
					case PRESERVE:
						stringBuilder.appendCodePoint(currentToken);
						break;
				}
			}
		}
		return stringBuilder.toString();
	}

	private InternalReadExtendedToken internalReadExtendedToken(final StringBuilder stringBuilder, final ReadResult firstChar,
																final boolean escapeFirstChar) {

		final List<Integer> escapes = new ArrayList<>();

		ReadResult firstResult = firstChar;

		if (escapeFirstChar) {
			escapes.add(stringBuilder.length() - 1);

			firstResult = readInternalToken(stringBuilder);
		}

		ReadResult readResult = firstResult;
		Integer colon = null;

		while (!readResult.wasEOF()) {

			final int codePoint = readResult.getResult();
			if (isSyntaxType(stateReader, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
				// TODO: We want to take "read-preserving-whitespace" into account here before unreading
				stateReader.unreadChar(codePoint);
				stringBuilder.deleteCharAt(stringBuilder.length() - 1);
				break;
			}

			if (isSyntaxType(stateReader, codePoint, SyntaxType.SINGLE_ESCAPE)) {
				final ReadResult nextReadResult = readInternalToken(stringBuilder);

				if (nextReadResult.wasEOF()) {
					throw new ReaderErrorException("EOF after escape character.");
				} else {
					escapes.add(stringBuilder.length() - 1);
				}
			} else if (isSyntaxType(stateReader, codePoint, SyntaxType.MULTIPLE_ESCAPE)) {
				while (true) {

					final ReadResult tempReadResult = readInternalToken(stringBuilder);

					if (tempReadResult.wasEOF()) {
						throw new ReaderErrorException("EOF inside extended token.");
					}

					final int tempCodePoint = tempReadResult.getResult();
					if (isSyntaxType(stateReader, tempCodePoint, SyntaxType.MULTIPLE_ESCAPE)) {
						break;
					} else if (isSyntaxType(stateReader, tempCodePoint, SyntaxType.SINGLE_ESCAPE)) {

						final ReadResult nextReadResult = readInternalToken(stringBuilder);

						if (nextReadResult.wasEOF()) {
							throw new ReaderErrorException("EOF after escape character.");
						} else {
							escapes.add(stringBuilder.length() - 1);
						}
					} else {
						escapes.add(stringBuilder.length() - 1);
					}
				}
			} else {
				if (isSyntaxType(stateReader, codePoint, SyntaxType.CONSTITUENT)
						&& isAttributeType(stateReader, codePoint, AttributeType.PACKAGEMARKER)
						&& (colon == null)) {
					colon = stringBuilder.length() - 1;
				}
			}

			readResult = readInternalToken(stringBuilder);
		}

		return new InternalReadExtendedToken(escapes, colon);
	}

	private ReadResult readInternalToken(final StringBuilder stringBuilder) {

		final ReadResult readResult = stateReader.readChar(false, null, true);
		if (!readResult.wasEOF()) {
			stringBuilder.appendCodePoint(readResult.getResult());
		}
		return readResult;
	}

	private static class InternalReadExtendedToken {

		private final List<Integer> escapeIndices;
		private final Integer firstPackageDelimiter;

		private InternalReadExtendedToken(final List<Integer> escapeIndices, final Integer firstPackageDelimiter) {
			this.escapeIndices = escapeIndices;
			this.firstPackageDelimiter = firstPackageDelimiter;
		}

		public List<Integer> getEscapeIndices() {
			return escapeIndices;
		}

		public Integer getFirstPackageDelimiter() {
			return firstPackageDelimiter;
		}
	}

	//************************//
	//** #R, #B, #O, and #X **//
	//************************//

	@Override
	public IntegerStruct readIntegerToken(final Integer radix) {
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			readExtendedToken();
			return null;
		} else if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		} else if ((radix < 2) && (radix > 36)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		} else {
			final int previousReadBase = ReadBaseVariable.INSTANCE.getValue();

			// alter the readbase
			ReadBaseVariable.INSTANCE.setValue(radix);

			// read integer
			final LispStruct lispToken = stateReader.read();
			if (lispToken instanceof IntegerStruct) {

				final IntegerStruct integerToken = (IntegerStruct) lispToken;

				// reset the readbase
				ReadBaseVariable.INSTANCE.setValue(previousReadBase);

				return integerToken;
			} else {
				// reset the readbase
				ReadBaseVariable.INSTANCE.setValue(previousReadBase);

				throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');
			}
		}
	}

	//***************//
	//** READ-LIST **//
	//***************//

	@Override
	public ListStruct readList() {

		final List<LispStruct> theList = new ArrayList<>();

		int codePoint = flushWhitespace();

		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {

				int nextCodePoint = stateReader.readChar().getResult();

				if (isSyntaxType(stateReader, nextCodePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
					if (theList.isEmpty()) {
						if (ReadSuppressVariable.INSTANCE.getValue()) {
							return null;
						} else {
							throw new ReaderErrorException("Nothing appears before . in list.");
						}
					}

					if (isSyntaxType(stateReader, nextCodePoint, SyntaxType.WHITESPACE)) {
						nextCodePoint = flushWhitespace();
					}

					final LispStruct lispStruct = readAfterDot(nextCodePoint);
					theList.add(lispStruct);
					return ListStruct.buildDottedList(theList);
				} else {
					stateReader.unreadChar(nextCodePoint);
				}
			}
			stateReader.unreadChar(codePoint);

			final LispStruct lispStruct = stateReader.read();
			if (lispStruct != null) {
				theList.add(lispStruct);
			}

			codePoint = flushWhitespace();
		}

		return ListStruct.buildProperList(theList);
	}

	private LispStruct readAfterDot(final int firstCodePoint) {

		LispStruct lispStruct;

		int codePoint = firstCodePoint;
		while (true) {

			if (codePoint == CharacterConstants.RIGHT_PARENTHESIS) {
				throw new ReaderErrorException("Nothing appears after . in list.");
			}
			stateReader.unreadChar(codePoint);

			lispStruct = stateReader.read();
			if (lispStruct != null) {
				break;
			}

			codePoint = flushWhitespace();
		}

		int nextCodePoint = flushWhitespace();
		while (nextCodePoint != CharacterConstants.RIGHT_PARENTHESIS) {
			stateReader.unreadChar(nextCodePoint);

			lispStruct = stateReader.read();
			if (lispStruct != null) {
				throw new ReaderErrorException("More than one object follows . in list.");
			}

			nextCodePoint = flushWhitespace();
		}

		return lispStruct;
	}

	private int flushWhitespace() {

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int codePoint = readResult.getResult();

		if (isSyntaxType(stateReader, codePoint, SyntaxType.WHITESPACE)) {
			readResult = stateReader.readChar();
			codePoint = readResult.getResult();
		}

		return codePoint;
	}

	//***********************//
	//** READ-UNICODE-CHAR **//
	//***********************//

	@Override
	public int readUnicodeChar() {

		final StringBuilder unicodeCharBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int readChar = readResult.getResult();

		while (!isSyntaxType(stateReader, readChar, SyntaxType.WHITESPACE)) {
			unicodeCharBuilder.appendCodePoint(readChar);

			readResult = stateReader.readChar();
			readChar = readResult.getResult();
		}

		final String unicodeCharString = unicodeCharBuilder.toString();
		try {
			final int codePoint = Integer.parseInt(unicodeCharString, 16);
			if (!Character.isValidCodePoint(codePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharString + " is not a valid code point.");
			}
			return codePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}

	//***************//
	//** #+ and #- **//
	//***************//

	@Override
	public void readFeatures(final boolean shouldHideFeatures) {

		boolean isFeature;

		final PackageStruct previousPackage = PackageVariable.INSTANCE.getValue();
		final boolean previousReadSuppress = ReadSuppressVariable.INSTANCE.getValue();
		try {
			PackageVariable.INSTANCE.setValue(GlobalPackageStruct.KEYWORD);
			ReadSuppressVariable.INSTANCE.setValue(false);

			final LispStruct token = stateReader.read();

			isFeature = isFeature(token);
		} catch (final ReaderErrorException ree) {
			LOGGER.debug(ree.getMessage(), ree);
			isFeature = false;
		} finally {
			PackageVariable.INSTANCE.setValue(previousPackage);
		}

		if (isFeature && shouldHideFeatures) {

			ReadSuppressVariable.INSTANCE.setValue(true);
			stateReader.read();
			ReadSuppressVariable.INSTANCE.setValue(previousReadSuppress);
		}
	}

	// TODO: We REALLY need to do this better at some point...
	private static boolean isFeature(final LispStruct token) {

		final boolean returnVal;

		if (token instanceof ConsStruct) {
			final ListStruct listStruct = (ListStruct) token;

			final LispStruct firstToken = listStruct.getFirst();
			final List<LispStruct> restTokens = listStruct.getRest().getAsJavaList();

			final SymbolStruct<?> symbolToken = (SymbolStruct<?>) firstToken;

			switch (symbolToken.getName().toUpperCase()) {
				case "NOT":
					returnVal = !isFeature(restTokens.get(0));
					break;
				case "AND":

					boolean tempReturnVal = true;
					for (final LispStruct lispToken : restTokens) {
						tempReturnVal = tempReturnVal && isFeature(lispToken);
					}
					returnVal = tempReturnVal;
					break;
				case "OR":

					boolean tempReturnVal2 = false;
					for (final LispStruct lispToken2 : restTokens) {
						tempReturnVal2 = tempReturnVal2 || isFeature(lispToken2);
					}
					returnVal = tempReturnVal2;
					break;
				default:
					throw new ReaderErrorException("Unknown operator in feature expression: " + symbolToken.getValue());
			}
		} else if (token instanceof SymbolStruct) {
			final SymbolStruct<?> symbolToken = (SymbolStruct<?>) token;

			final List<SymbolStruct<?>> featuresList = FeaturesVariable.INSTANCE.getValue();
			returnVal = featuresList.contains(symbolToken);
		} else {
			throw new ReaderErrorException("");
		}

		return returnVal;
	}

	// UTILITIES

	private static boolean isAttributeType(final StateReader stateReader, final int codePoint, final AttributeType... attributeTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final AttributeType attributeType : attributeTypes) {
			returnVal = returnVal || (readtable.getAttributeType(codePoint) == attributeType);
		}
		return returnVal;
	}

	private static boolean isSyntaxType(final StateReader stateReader, final int codePoint, final SyntaxType... syntaxTypes) {

		final ReadtableStruct readtable = stateReader.getReadtable();

		boolean returnVal = false;
		for (final SyntaxType syntaxType : syntaxTypes) {
			returnVal = returnVal || (readtable.getSyntaxType(codePoint) == syntaxType);
		}
		return returnVal;
	}
}
