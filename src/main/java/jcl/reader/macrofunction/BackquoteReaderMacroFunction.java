/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.functions.ListToVectorFunction;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '`' Lisp reader macro.
 */
@Component
public class BackquoteReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CharacterConstants.GRAVE_ACCENT, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		reader.incrementBackquoteLevel();
		try {
			final LispStruct code = reader.read(true, NILStruct.INSTANCE, true);
			final BackquoteReturn backquoteReturn = backquotify(code);

			final SymbolStruct flag = backquoteReturn.getFlag();
			final LispStruct thing = backquoteReturn.getThing();

			if (CommonLispSymbols.BQ_AT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",@ after backquote in " + printer.print(thing));
			}
			if (CommonLispSymbols.BQ_DOT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",. after backquote in " + printer.print(thing));
			}

			return backquotify_1(flag, thing);
		} finally {
			reader.decrementBackquoteLevel();
		}
	}

	private BackquoteReturn backquotify(final LispStruct code) {

		if (code instanceof NILStruct) {
			return new BackquoteReturn(NILStruct.INSTANCE, code);
		}

		if (code instanceof SymbolStruct) {
			// Need to check the constant symbols here
			final SymbolStruct codeSymbol = (SymbolStruct) code;
			if (TStruct.INSTANCE.equals(codeSymbol)) {
				return new BackquoteReturn(TStruct.INSTANCE, code);
			}
			if (NILStruct.INSTANCE.equals(codeSymbol)) {
				return new BackquoteReturn(NILStruct.INSTANCE, code);
			}
			return new BackquoteReturn(CommonLispSymbols.QUOTE, code);
		}

		if (code instanceof ConsStruct) {
			final ConsStruct consCode = (ConsStruct) code;

			final LispStruct carConsCode = consCode.getCar();
			final LispStruct cdrConsCode = consCode.getCdr();

			if (CommonLispSymbols.BQ_AT_FLAG.equals(carConsCode) || CommonLispSymbols.BQ_DOT_FLAG.equals(carConsCode)) {
				final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
				return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
			}

			if (CommonLispSymbols.BQ_COMMA_FLAG.equals(carConsCode)) {
				return comma(cdrConsCode);
			}

			if (CommonLispSymbols.BQ_VECTOR_FLAG.equals(carConsCode)) {
				final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

				final SymbolStruct cdrBqtifyFlag = cdrBqtify.getFlag();
				final LispStruct cdrBqtifyThing = cdrBqtify.getThing();

				final LispStruct cdrBqtify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				return new BackquoteReturn(CommonLispSymbols.LIST_TO_VECTOR, cdrBqtify_1);
			}

			final BackquoteReturn carBqtify = backquotify(carConsCode);

			final SymbolStruct carBqtifyFlag = carBqtify.getFlag();
			final LispStruct carBqtifyThing = carBqtify.getThing();

			final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

			final SymbolStruct cdrBqtifyFlag = cdrBqtify.getFlag();
			final LispStruct cdrBqtifyThing = cdrBqtify.getThing();

			if (CommonLispSymbols.BQ_AT_FLAG.equals(cdrBqtifyFlag)) {
				throw new ReaderErrorException(",@ after dot in " + printer.print(code));
			}

			if (CommonLispSymbols.BQ_DOT_FLAG.equals(cdrBqtifyFlag)) {
				throw new ReaderErrorException(",. after dot in " + printer.print(code));
			}

			if (CommonLispSymbols.BQ_AT_FLAG.equals(carBqtifyFlag)) {
				return backquotifyAtFlag(carBqtifyThing, cdrBqtifyFlag, cdrBqtifyThing);
			}

			if (CommonLispSymbols.BQ_DOT_FLAG.equals(carBqtifyFlag)) {
				return backquotifyDotFlag(carBqtifyThing, cdrBqtifyFlag, cdrBqtifyThing);
			}

			if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {

				if (CommonLispSymbols.QUOTE.equals(carBqtifyFlag)
						|| TStruct.INSTANCE.equals(carBqtifyFlag)
						|| NILStruct.INSTANCE.equals(carBqtifyFlag)) {

					final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
					return new BackquoteReturn(CommonLispSymbols.QUOTE, bqReturnThing);
				} else {

					final LispStruct backquotify_1 = backquotify_1(carBqtifyFlag, carBqtifyThing);
					final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1);

					return new BackquoteReturn(CommonLispSymbols.LIST, bqReturnThing);
				}
			}

			if (CommonLispSymbols.QUOTE.equals(cdrBqtifyFlag)
					|| TStruct.INSTANCE.equals(cdrBqtifyFlag)) {

				if (CommonLispSymbols.QUOTE.equals(carBqtifyFlag)
						|| TStruct.INSTANCE.equals(carBqtifyFlag)
						|| NILStruct.INSTANCE.equals(carBqtifyFlag)) {

					final ConsStruct bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
					return new BackquoteReturn(CommonLispSymbols.QUOTE, bqReturnThing);
				} else {

					final LispStruct backquotify_1_a = backquotify_1(carBqtifyFlag, carBqtifyThing);
					final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1_a, backquotify_1_d);

					return new BackquoteReturn(CommonLispSymbols.LIST_STAR, bqReturnThing);
				}
			}

			final LispStruct nextCarBqtifyThing = backquotify_1(carBqtifyFlag, carBqtifyThing);

			if (CommonLispSymbols.LIST.equals(cdrBqtifyFlag)
					|| CommonLispSymbols.LIST_STAR.equals(cdrBqtifyFlag)) {
				final ListStruct bqReturnThing = new ConsStruct(nextCarBqtifyThing, cdrBqtifyThing);
				return new BackquoteReturn(cdrBqtifyFlag, bqReturnThing);
			}

			final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
			final ListStruct bqReturnThing = ListStruct.buildProperList(nextCarBqtifyThing, backquotify_1_d);

			return new BackquoteReturn(CommonLispSymbols.LIST_STAR, bqReturnThing);
		}

		return new BackquoteReturn(TStruct.INSTANCE, code);
	}

	private BackquoteReturn backquotifyAtFlag(final LispStruct carBqtifyThing, final SymbolStruct cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {
			if (expandableBackqExpressionP(carBqtifyThing)) {
				final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
				return new BackquoteReturn(CommonLispSymbols.APPEND, bqReturnThing);
			} else {
				return comma(carBqtifyThing);
			}
		} else {

			final ListStruct bqReturnThing;
			if (CommonLispSymbols.APPEND.equals(cdrBqtifyFlag)) {
				bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
			} else {
				final LispStruct backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
			}

			return new BackquoteReturn(CommonLispSymbols.APPEND, bqReturnThing);
		}
	}

	private BackquoteReturn backquotifyDotFlag(final LispStruct carBqtifyThing, final SymbolStruct cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {
			if (expandableBackqExpressionP(carBqtifyThing)) {
				final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
				return new BackquoteReturn(CommonLispSymbols.NCONC, bqReturnThing);
			} else {
				return comma(carBqtifyThing);
			}
		} else {

			final ListStruct bqReturnThing;
			if (CommonLispSymbols.NCONC.equals(cdrBqtifyFlag)) {
				bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
			} else {
				final LispStruct backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
			}

			return new BackquoteReturn(CommonLispSymbols.NCONC, bqReturnThing);
		}
	}

	private static BackquoteReturn comma(final LispStruct code) {

		if (code instanceof NILStruct) {
			return new BackquoteReturn(NILStruct.INSTANCE, code);
		}

		if (code instanceof NumberStruct) {
			return new BackquoteReturn(TStruct.INSTANCE, code);
		}

		if (code instanceof SymbolStruct) {
			// Need to check the constant symbols here
			final SymbolStruct codeSymbol = (SymbolStruct) code;
			if (TStruct.INSTANCE.equals(codeSymbol)) {
				return new BackquoteReturn(TStruct.INSTANCE, code);
			} else if (NILStruct.INSTANCE.equals(codeSymbol)) {
				return new BackquoteReturn(NILStruct.INSTANCE, code);
			}
		}

		if (code instanceof ConsStruct) {
			final ConsStruct consCode = (ConsStruct) code;

			final LispStruct carConsCode = consCode.getCar();
			final LispStruct cdrConsCode = consCode.getCdr();

			if (CommonLispSymbols.QUOTE.equals(carConsCode)) {
				// NOTE: This cast will always be fine because of how we build the ConsStruct in the CommaReaderMacroFunction
				final LispStruct cadrConsCode = ((ConsStruct) cdrConsCode).getCar();
				if (!expandableBackqExpressionP(cadrConsCode)) {
					final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
					return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
				}
			}

			if (CommonLispSymbols.APPEND.equals(carConsCode)
					|| CommonLispSymbols.LIST.equals(carConsCode)
					|| CommonLispSymbols.NCONC.equals(carConsCode)) {

				final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
				return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
			}

			if (CommonLispSymbols.CONS.equals(carConsCode)
					|| CommonLispSymbols.LIST_STAR.equals(carConsCode)) {

				return new BackquoteReturn(CommonLispSymbols.LIST_STAR, cdrConsCode);
			}
		}

		return new BackquoteReturn(CommonLispSymbols.BQ_COMMA_FLAG, code);
	}

	private static boolean expandableBackqExpressionP(final LispStruct o) {
		if (o instanceof ConsStruct) {
			final ConsStruct consStruct = (ConsStruct) o;
			final LispStruct flag = consStruct.getCar();
			if (CommonLispSymbols.BQ_AT_FLAG.equals(flag) || CommonLispSymbols.BQ_DOT_FLAG.equals(flag)) {
				return true;
			}
		}
		return false;
	}

	private static LispStruct backquotify_1(final SymbolStruct flag, final LispStruct thing) {

		if (CommonLispSymbols.BQ_COMMA_FLAG.equals(flag) || TStruct.INSTANCE.equals(flag) || NILStruct.INSTANCE.equals(flag)) {
			return thing;
		}

		if (CommonLispSymbols.QUOTE.equals(flag)) {
			return ListStruct.buildProperList(CommonLispSymbols.QUOTE, thing);
		}

		if (CommonLispSymbols.LIST_STAR.equals(flag)) {
			// NOTE: The following check is not in CMU-CL. Actually a semi-bug found when handling improperly created lists.
			if (thing instanceof ConsStruct) {
				final ConsStruct consThing = (ConsStruct) thing;

				final LispStruct cdrThing = consThing.getCdr();

				// NOTE: This will always be an ok cast due to the backquote reader algorithm
				final LispStruct cadrThing = ((ConsStruct) cdrThing).getCar();
				final LispStruct cddrThing = ((ConsStruct) cdrThing).getCdr();

				if (NILStruct.INSTANCE.equals(cddrThing) && !expandableBackqExpressionP(cadrThing)) {
					// Basically if there are only 2 items in the list, just use Cons function
					return new ConsStruct(CommonLispSymbols.CONS, thing);
				}

				// NOTE: This is a safe cast, as the last() will always return a List with the last cons, even if the list is dotted
				final ListStruct lastThing = (ListStruct) consThing.last();
				final LispStruct carOfLastThing = lastThing.getCar();

				if (expandableBackqExpressionP(carOfLastThing)) {
					final ListStruct allButLast = consThing.butLast();
					final ConsStruct consStruct = new ConsStruct(CommonLispSymbols.LIST, allButLast);

					return ListStruct.buildProperList(CommonLispSymbols.APPEND, consStruct, carOfLastThing);
				}
			}

			return new ConsStruct(CommonLispSymbols.LIST_STAR, thing);
		}

		if (CommonLispSymbols.LIST_TO_VECTOR.equals(flag)) {
			return ListStruct.buildProperList(CommonLispSymbols.LIST_TO_VECTOR, thing);
		}

		return new ConsStruct(flag, thing);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(printer)
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
		final BackquoteReaderMacroFunction rhs = (BackquoteReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}

	private static final class BackquoteReturn {

		private final SymbolStruct flag;

		private final LispStruct thing;

		private BackquoteReturn(final SymbolStruct flag, final LispStruct thing) {
			this.flag = flag;
			this.thing = thing;
		}

		public SymbolStruct getFlag() {
			return flag;
		}

		public LispStruct getThing() {
			return thing;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(flag)
			                            .append(thing)
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
			final BackquoteReturn rhs = (BackquoteReturn) obj;
			return new EqualsBuilder().append(flag, rhs.flag)
			                          .append(thing, rhs.thing)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(flag)
			                                                                .append(thing)
			                                                                .toString();
		}
	}
}
