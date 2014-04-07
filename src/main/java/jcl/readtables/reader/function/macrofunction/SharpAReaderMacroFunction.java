package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

import java.util.ArrayList;
import java.util.List;

/**
 * Implements the '#a' Lisp reader macro.
 */
public class SharpAReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct contents = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
	private static List<Integer> getDimensions(final int dimensions, final LispStruct contents) {

		LispStruct seq = contents;
		Integer zeroAxis = null;

		final List<Integer> dims = new ArrayList<>();

		for (int axis = 0; axis < dimensions; axis++) {

			final ListStruct seqList;
			if (seq instanceof ListStruct) {
				seqList = (ListStruct) seq;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + seq);
			}

			final List<LispStruct> lispTokens = seqList.getAsJavaList();

			final int seqLength = lispTokens.size();
			dims.add(seqLength);

			if (axis != (dimensions - 1)) {
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
