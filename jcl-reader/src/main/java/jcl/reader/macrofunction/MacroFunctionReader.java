package jcl.reader.macrofunction;

import jcl.reader.LispReader;
import jcl.reader.state.StateReader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.syntax.SyntaxType;
import jcl.reader.util.ReaderUtils;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conses.ConsStruct;
import jcl.structs.conses.ListStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.readtables.ReadtableStruct;
import jcl.structs.streams.ReadResult;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.Variable;
import org.apache.commons.collections4.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MacroFunctionReader extends LispReader {

	private final StateReader stateReader;

	public MacroFunctionReader(final StateReader stateReader) {
		this.stateReader = stateReader;
	}

	@Override
	public LispStruct read() throws ReaderErrorException {
		return stateReader.read();
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP)
			throws ReaderErrorException {
		return stateReader.read(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadResult readChar() throws ReaderErrorException {
		return stateReader.readChar();
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws ReaderErrorException {
		return stateReader.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) throws ReaderErrorException {
		stateReader.unreadChar(codePoint);
	}

	//*************************//
	//** READ-EXTENDED-TOKEN **//
	//*************************//

	public ReadExtendedToken readExtendedToken() throws ReaderErrorException {

		final StringBuilder stringBuilder = new StringBuilder();
		final ReadResult readResult = readInternalToken(stringBuilder);

		if (readResult.wasEOF()) {
			return new ReadExtendedToken("", false, false);
		} else {
			final InternalReadExtendedToken readExtendedToken = internalReadExtendedToken(stringBuilder, readResult, false);

			final List<Integer> escapeIndices = readExtendedToken.getEscapeIndices();
			final Integer colon = readExtendedToken.getFirstPackageDelimiter();

			final String tokenWithProperCase = getTokenWithProperCase(stateReader.getReadtable(), stringBuilder, escapeIndices);

			return new ReadExtendedToken(tokenWithProperCase, CollectionUtils.isNotEmpty(escapeIndices), colon != null);
		}
	}

	public String readExtendedTokenEscaped() throws ReaderErrorException {

		final StringBuilder stringBuilder = new StringBuilder();
		final ReadResult readResult = readInternalToken(stringBuilder);

		if (readResult.wasEOF()) {
			throw new ReaderErrorException("EOF after escape character.");
		} else {
			final InternalReadExtendedToken readExtendedToken = internalReadExtendedToken(stringBuilder, readResult, true);

			final List<Integer> escapeIndices = readExtendedToken.getEscapeIndices();

			return getTokenWithProperCase(stateReader.getReadtable(), stringBuilder, escapeIndices);
		}
	}

	private static String getTokenWithProperCase(final ReadtableStruct readtable, final StringBuilder token, final List<Integer> escapeIndices) {
		final CaseSpec readtableCase = readtable.getReadtableCase();

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
																final boolean escapeFirstChar)
			throws ReaderErrorException {

		final ReadtableStruct readtable = stateReader.getReadtable();

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
			if (ReaderUtils.isSyntaxType(readtable, codePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
				// TODO: We want to take "read-preserving-whitespace" into account here before unreading
				stateReader.unreadChar(codePoint);
				stringBuilder.deleteCharAt(stringBuilder.length() - 1);
				break;
			}

			if (ReaderUtils.isSyntaxType(readtable, codePoint, SyntaxType.SINGLE_ESCAPE)) {
				final ReadResult nextReadResult = readInternalToken(stringBuilder);

				if (nextReadResult.wasEOF()) {
					throw new ReaderErrorException("EOF after escape character.");
				} else {
					escapes.add(stringBuilder.length() - 1);
				}
			} else if (ReaderUtils.isSyntaxType(readtable, codePoint, SyntaxType.MULTIPLE_ESCAPE)) {
				while (true) {

					final ReadResult tempReadResult = readInternalToken(stringBuilder);

					if (tempReadResult.wasEOF()) {
						throw new ReaderErrorException("EOF inside extended token.");
					}

					final int tempCodePoint = tempReadResult.getResult();
					if (ReaderUtils.isSyntaxType(readtable, tempCodePoint, SyntaxType.MULTIPLE_ESCAPE)) {
						break;
					} else if (ReaderUtils.isSyntaxType(readtable, tempCodePoint, SyntaxType.SINGLE_ESCAPE)) {

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
				if (ReaderUtils.isSyntaxType(readtable, codePoint, SyntaxType.CONSTITUENT)
						&& ReaderUtils.isAttributeType(readtable, codePoint, AttributeType.PACKAGEMARKER)
						&& (colon == null)) {
					colon = stringBuilder.length() - 1;
				}
			}

			readResult = readInternalToken(stringBuilder);
		}

		return new InternalReadExtendedToken(escapes, colon);
	}

	private ReadResult readInternalToken(final StringBuilder stringBuilder) throws ReaderErrorException {

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

	public IntegerStruct readIntegerToken(final Integer radix) throws ReaderErrorException {
		if (Variable.ReadSuppress) {
			readExtendedToken();
			return null;
		} else if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		} else if ((radix < 2) && (radix > 36)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		} else {
			final int previousReadBase = Variable.getReadBase();

			// alter the readbase
			Variable.setReadBase(radix);

			// read integer
			final LispStruct lispToken = stateReader.read();
			if (lispToken instanceof IntegerStruct) {

				final IntegerStruct integerToken = (IntegerStruct) lispToken;

				// reset the readbase
				Variable.setReadBase(previousReadBase);

				return integerToken;
			} else {
				// reset the readbase
				Variable.setReadBase(previousReadBase);

				throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');
			}
		}
	}

	//***************//
	//** READ-LIST **//
	//***************//

	public ListStruct readList() throws ReaderErrorException {

		final ReadtableStruct readtable = stateReader.getReadtable();

		final List<LispStruct> theList = new ArrayList<>();

		int codePoint = flushWhitespace();

		while (codePoint != CharacterConstants.RIGHT_PARENTHESIS) {

			if (codePoint == CharacterConstants.FULL_STOP) {

				int nextCodePoint = stateReader.readChar().getResult();

				if (ReaderUtils.isSyntaxType(readtable, nextCodePoint, SyntaxType.WHITESPACE, SyntaxType.TERMINATING)) {
					if (theList.isEmpty()) {
						if (Variable.ReadSuppress) {
							return null;
						} else {
							throw new ReaderErrorException("Nothing appears before . in list.");
						}
					}

					if (ReaderUtils.isSyntaxType(readtable, nextCodePoint, SyntaxType.WHITESPACE)) {
						nextCodePoint = flushWhitespace();
					}

					final LispStruct lispStruct = readAfterDot(nextCodePoint);
					theList.add(lispStruct);
					return ListStruct.getStruct(true, theList);
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

		if (theList.isEmpty()) {
			return ListStruct.getStruct();
		} else {
			return ListStruct.getStruct(theList);
		}
	}

	private LispStruct readAfterDot(final int firstCodePoint) throws ReaderErrorException {

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

	private int flushWhitespace() throws ReaderErrorException {

		final ReadtableStruct readtable = stateReader.getReadtable();

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int codePoint = readResult.getResult();

		if (ReaderUtils.isSyntaxType(readtable, codePoint, SyntaxType.WHITESPACE)) {
			readResult = stateReader.readChar();
			codePoint = readResult.getResult();
		}

		return codePoint;
	}

	//***********************//
	//** READ-UNICODE-CHAR **//
	//***********************//

	public int readUnicodeChar() throws ReaderErrorException {

		final ReadtableStruct readtable = stateReader.getReadtable();

		final StringBuilder unicodeCharBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int readChar = readResult.getResult();

		while (!ReaderUtils.isSyntaxType(readtable, readChar, SyntaxType.WHITESPACE)) {
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

	public void readFeatures(final boolean shouldHideFeatures) throws ReaderErrorException {

		boolean isFeature;

		final PackageStruct previousPackage = Variable.getPackage();
		final boolean previousReadSuppress = Variable.isReadSuppress();
		try {
			Variable.setPackage(PackageStruct.KEYWORD);
			Variable.setReadSuppress(false);

			final LispStruct token = stateReader.read();

			isFeature = isFeature(token);
		} catch (final ReaderErrorException ignore) {
			isFeature = false;
		} finally {
			Variable.setPackage(previousPackage);
		}

		if (isFeature && shouldHideFeatures) {

			Variable.setReadSuppress(true);
			stateReader.read();
			Variable.setReadSuppress(previousReadSuppress);
		}
	}

	// TODO: We REALLY need to do this better at some point...
	private static boolean isFeature(final LispStruct token) throws ReaderErrorException {

		final boolean returnVal;

		if (token instanceof ConsStruct) {
			final ListStruct listStruct = (ListStruct) token;

			final LispStruct firstToken = listStruct.getFirst();
			final List<LispStruct> restTokens = listStruct.getRest();

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

			final List<SymbolStruct<?>> featuresList = Variable.Features;
			returnVal = featuresList.contains(symbolToken);
		} else {
			throw new ReaderErrorException("");
		}

		return returnVal;
	}

	//***************//
	//** #= and ## **//
	//***************//

	public static final Map<Integer, LispStruct> SHARP_EQUAL_FINAL_TABLE = new HashMap<>();
	public static final Map<Integer, Object> SHARP_EQUAL_TEMP_TABLE = new HashMap<>();
	public static final Map<Integer, LispStruct> SHARP_EQUAL_REPL_TABLE = new HashMap<>();
	public static final Map<Integer, LispStruct> SHARP_EQUAL_CIRCLE_TABLE = new HashMap<>();

	public void circleSubst(final Map<Long, LispStruct> replTable, final LispStruct tree) {
/*
;; This function is kind of like to NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists.  The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
;;
(defun circle-subst (repl-table tree)
  (cond ((not (typep tree '(or cons (array t) structure-object standard-object)))
	     (multiple-value-bind (value presentp)
	                          (gethash tree repl-table)
	       (if presentp
	           value
	         tree)))
		((null (gethash tree *sharp-equal-circle-table*))
	     (setf (gethash tree *sharp-equal-circle-table*) t)
	     (cond ((typep tree '(or structure-object standard-object))
				(do ((i 1 (1+ i))
		             (end (%instance-length tree)))
		            ((= i end))
		         (let* ((old (%instance-ref tree i))
			            (new (circle-subst repl-table old)))
		           (unless (eq old new)
		             (setf (%instance-ref tree i) new)))))
	           ((arrayp tree)
				(with-array-data ((data tree) (start) (end))
		          (declare (fixnum start end))
		          (do ((i start (1+ i)))
		              ((>= i end))
		            (let* ((old (aref data i))
			               (new (circle-subst repl-table old)))
		              (unless (eq old new)
						(setf (aref data i) new))))))
	           (t
				(let ((a (circle-subst repl-table (car tree)))
		              (d (circle-subst repl-table (cdr tree))))
		          (unless (eq a (car tree))
		            (rplaca tree a))
		          (unless (eq d (cdr tree))
		            (rplacd tree d)))))
	     tree)
		(t
		 tree)))
*/
	}
}
