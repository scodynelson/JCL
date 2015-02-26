/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SequenceElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements the '#a' Lisp reader macro.
 */
@Component
public class SharpAReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3446360583440445990L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpAReaderMacroFunction.class);

	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_A, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_A, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final SimpleElement lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				final String printedToken = printer.print(lispToken);
				LOGGER.debug("{} suppressed.", printedToken);
			}
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}

		if (BigInteger.ZERO.compareTo(numArg) > 0) {
			if (!(lispToken instanceof SequenceElement)) {
				// TODO: this is NOT adequate!! Because we MAKE ListElements in other parts of the reader...
				final String printedToken = printer.print(lispToken);
				throw new ReaderErrorException("The form following a #" + numArg + "A reader macro should have been a sequence, but it was: " + printedToken);
			}
		}

		return createArray(numArg, lispToken);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * ArrayStruct} from the provided {@code contents}.
	 *
	 * @param numArg
	 * 		the number of expected dimensions
	 * @param contents
	 * 		the {@link LispStruct} tokens used to create the {@link ArrayStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link ArrayStruct}
	 */
	private static ConsElement createArray(final BigInteger numArg, final SimpleElement contents) {

		final List<SimpleElement> dimensionsAsJavaList = getDimensions(numArg, contents);

		final SymbolElement makeArrayFnSymbol = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "MAKE-ARRAY");

		final SimpleElement dimensions;
		if (dimensionsAsJavaList.isEmpty()) {
			dimensions = NullElement.INSTANCE;
		} else {
			dimensions = new ConsElement(dimensionsAsJavaList);
		}

		final SymbolElement elementTypeKeyword = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "ELEMENT-TYPE");
		final SymbolElement elementType = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "T");
		final SymbolElement initialContentsKeyword = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "INITIAL-CONTENTS");

		final SimpleElement initialContents;
		if (contents instanceof SequenceElement) {
			final SequenceElement sequenceContents = (SequenceElement) contents;
			final List<? extends SimpleElement> sequenceContentsAsJavaList = sequenceContents.getElements();

			if (sequenceContentsAsJavaList.isEmpty()) {
				initialContents = NullElement.INSTANCE;
			} else {
				initialContents = new ConsElement(contents);
			}
		} else {
			initialContents = contents;
		}

		return new ConsElement(makeArrayFnSymbol, dimensions, elementTypeKeyword, elementType, initialContentsKeyword, initialContents);
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
	 * @return a List of {@link IntegerStruct} that make up the expected dimensions of the simple array
	 *
	 * @throws ReaderErrorException
	 * 		if dimensions do not match the provided contents list
	 */
	private static List<SimpleElement> getDimensions(final BigInteger dimensions, final SimpleElement contents) {

		SimpleElement seq = contents;
		BigInteger zeroAxis = null;

		final List<SimpleElement> dimensionsAsJavaList = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     dimensions.compareTo(axis) >= 0;
		     axis = axis.add(BigInteger.ONE)) {

			final SequenceElement seqList;
			if (seq instanceof SequenceElement) {
				seqList = (SequenceElement) seq;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + seq);
			}

			final List<? extends SimpleElement> lispTokens = seqList.getElements();

			final int seqLength = lispTokens.size();
			final BigInteger seqLengthBI = BigInteger.valueOf(seqLength);
			final IntegerElement dimension = new IntegerElement(seqLengthBI);
			dimensionsAsJavaList.add(dimension);

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

		return dimensionsAsJavaList;
	}
}
