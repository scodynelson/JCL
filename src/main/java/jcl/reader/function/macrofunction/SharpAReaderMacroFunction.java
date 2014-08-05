package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.arrays.ArrayStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.Variable;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements the '#a' Lisp reader macro.
 */
public class SharpAReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct contents = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}

		if (!(contents instanceof ListStruct)) {
			throw new ReaderErrorException("#" + numArg + "A axis " + 0 + " is not a sequence: " + contents);
		}

		final ListStruct contentsList = (ListStruct) contents;
		final List<LispStruct> contentsAsJavaList = contentsList.getAsJavaList();

		final List<Integer> dims = getDimensions(numArg, contentsList);
		try {
			return new ArrayStruct<>(dims, contentsAsJavaList);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating array.", e);
		}
	}

	/**
	 * This method will build the dimensions list from the contents of the provided ListStruct. The dimensions will be based
	 * off of the first element in each axis layer of the supplied dimensions parameter.
	 *
	 * @param dimensions the number of expected dimensions
	 * @param contents   the contents to build and analyze against the dimensions parameter
	 * @return a List of Integers that make up the expected dimensions of the simple array
	 * @throws ReaderErrorException if dimensions do not match the provided contents list
	 */
	private static List<Integer> getDimensions(final BigInteger dimensions, final LispStruct contents) {

		LispStruct seq = contents;
		BigInteger zeroAxis = null;

		final List<Integer> dims = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     axis.compareTo(dimensions) < 0;
		     axis = axis.add(BigInteger.ONE)) {

			final ListStruct seqList;
			if (seq instanceof ListStruct) {
				seqList = (ListStruct) seq;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + seq);
			}

			final List<LispStruct> lispTokens = seqList.getAsJavaList();

			final int seqLength = lispTokens.size();
			dims.add(seqLength);

			if (!axis.equals(dimensions.subtract(BigInteger.ONE))) {
				if (seqLength == 0) {
					zeroAxis = axis;
				} else if (zeroAxis != null) {
					throw new ReaderErrorException("#" + dimensions + "A axis " + zeroAxis + " is empty, but axis " + axis + " is non-empty.");
				} else {
					seq = lispTokens.get(0);
				}
			}
		}

		return dims;
	}
}
