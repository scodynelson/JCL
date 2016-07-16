/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RationalStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.NILStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Reader Macro Function for handling the reading of {@link RationalStruct}s, following proper radix rules for a
 * provided radix value.
 */
@Component
final class RationalReaderMacroFunction {

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * Read in and returns a properly parsed {@link RationalStruct}, handling proper radix rules for reader number base
	 * handling based on the provided {@code radix} value.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the {@link RationalStruct}
	 * @param radix
	 * 		the radix value used when reading numeric parts of the {@link RationalStruct}
	 *
	 * @return the properly parsed {@link RationalStruct}
	 */
	LispStruct readRational(final Reader reader, final BigInteger radix) {
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
			return NILStruct.INSTANCE;
		}

		final IntegerStruct previousReadBase = ReaderVariables.READ_BASE.getVariableValue();

		// alter the read-base
		ReaderVariables.READ_BASE.setValue(IntegerStruct.valueOf(radix));

		// read rational
		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);

		// reset the read-base
		ReaderVariables.READ_BASE.setValue(previousReadBase);

		if (token instanceof RationalStruct) {
			return token;
		}

		final String printedToken = printer.print(token);
		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + printedToken + '.');
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final RationalReaderMacroFunction rhs = (RationalReaderMacroFunction) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
