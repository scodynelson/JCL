/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.lists.ListStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements the '#a' Lisp reader macro.
 */
public final class SharpAReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpAReaderMacroFunction INSTANCE = new SharpAReaderMacroFunction();

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpAReaderMacroFunction.class);

	/**
	 * Private constructor.
	 */
	private SharpAReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", lispToken.printStruct());
			}
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}

		if (!(lispToken instanceof ListStruct)) {
			throw new ReaderErrorException("#" + numArg + "A axis " + 0 + " is not a sequence: " + lispToken);
		}

		final ListStruct contents = (ListStruct) lispToken;
		final List<LispStruct> contentsAsJavaList = contents.getAsJavaList();

		final List<Integer> dims = getDimensions(numArg, contents);
		try {
			return new ArrayStruct<>(dims, contentsAsJavaList);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating array.", e);
		}
	}

	/**
	 * Build the dimensions list from the contents of the provided {@link LispStruct}. The dimensions will be based off
	 * of the first element in each axis layer of the supplied {@code dimensions} parameter.
	 *
	 * @param dimensions
	 * 		the number of expected dimensions
	 * @param contents
	 * 		the contents to build and analyze against the {@code dimensions} parameter
	 *
	 * @return a List of {@link Integer} that make up the expected dimensions of the simple array
	 *
	 * @throws ReaderErrorException
	 * 		if dimensions do not match the provided contents list
	 */
	private static List<Integer> getDimensions(final BigInteger dimensions, final LispStruct contents) {

		LispStruct seq = contents;
		BigInteger zeroAxis = null;

		final List<Integer> dims = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     dimensions.compareTo(axis) >= 0;
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
				if (lispTokens.isEmpty()) {
					zeroAxis = axis;
				} else if (zeroAxis == null) {
					seq = lispTokens.get(0);
				} else {
					throw new ReaderErrorException("#" + dimensions + "A axis " + zeroAxis + " is empty, but axis " + axis + " is non-empty.");
				}
			}
		}

		return dims;
	}
}
