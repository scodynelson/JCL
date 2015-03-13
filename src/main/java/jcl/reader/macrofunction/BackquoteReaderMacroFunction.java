/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.NumberStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '`' Lisp reader macro.
 */
@Component
public class BackquoteReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7900660772057166319L;

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
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.GRAVE_ACCENT, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numberArgument) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		reader.incrementBackquoteLevel();
		try {
			final LispStruct code = reader.read(true, NullStruct.INSTANCE, true);
			final BackquoteReturn backquoteReturn = backquotify(code);

			final SymbolStruct<?> flag = backquoteReturn.getFlag();
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

		if (code instanceof NullStruct) {
			return new BackquoteReturn(CommonLispSymbols.NIL, code);
		}

		if (code instanceof SymbolStruct<?>) {
			// Need to check the constant symbols here
			final SymbolStruct<?> codeSymbol = (SymbolStruct<?>) code;
			if (CommonLispSymbols.T.equals(codeSymbol)) {
				return new BackquoteReturn(CommonLispSymbols.T, code);
			}
			if (CommonLispSymbols.NIL.equals(codeSymbol)) {
				return new BackquoteReturn(CommonLispSymbols.NIL, code);
			}
			return new BackquoteReturn(CommonLispSymbols.QUOTE, code);
		}

		if (code instanceof ConsStruct) {
			final ConsStruct consCode = (ConsStruct) code;

			final LispStruct carConsCode = consCode.getCar();
			final LispStruct cdrConsCode = consCode.getCdr();

			if (CommonLispSymbols.BQ_AT_FLAG.equals(carConsCode) || CommonLispSymbols.BQ_DOT_FLAG.equals(carConsCode)) {
				final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
				return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
			}

			if (CommonLispSymbols.BQ_COMMA_FLAG.equals(carConsCode)) {
				return comma(cdrConsCode);
			}

			final BackquoteReturn carBqtify = backquotify(carConsCode);

			final SymbolStruct<?> carBqtifyFlag = carBqtify.getFlag();
			final LispStruct carBqtifyThing = carBqtify.getThing();

			final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

			final SymbolStruct<?> cdrBqtifyFlag = cdrBqtify.getFlag();
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

			if (CommonLispSymbols.NIL.equals(cdrBqtifyFlag)) {

				if (CommonLispSymbols.QUOTE.equals(carBqtifyFlag)
						|| CommonLispSymbols.T.equals(carBqtifyFlag)
						|| CommonLispSymbols.NIL.equals(carBqtifyFlag)) {

					final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
					return new BackquoteReturn(CommonLispSymbols.QUOTE, bqReturnThing);
				} else {

					final LispStruct backquotify_1 = backquotify_1(carBqtifyFlag, carBqtifyThing);
					final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1);

					return new BackquoteReturn(CommonLispSymbols.LIST, bqReturnThing);
				}
			}

			if (CommonLispSymbols.QUOTE.equals(cdrBqtifyFlag)
					|| CommonLispSymbols.T.equals(cdrBqtifyFlag)) {

				if (CommonLispSymbols.QUOTE.equals(carBqtifyFlag)
						|| CommonLispSymbols.T.equals(carBqtifyFlag)
						|| CommonLispSymbols.NIL.equals(carBqtifyFlag)) {

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

		return new BackquoteReturn(CommonLispSymbols.T, code);
	}

	private BackquoteReturn backquotifyAtFlag(final LispStruct carBqtifyThing, final SymbolStruct<?> cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (CommonLispSymbols.NIL.equals(cdrBqtifyFlag)) {
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

	private BackquoteReturn backquotifyDotFlag(final LispStruct carBqtifyThing, final SymbolStruct<?> cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (CommonLispSymbols.NIL.equals(cdrBqtifyFlag)) {
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

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	private static BackquoteReturn comma(final LispStruct code) {

		if (code instanceof NullStruct) {
			return new BackquoteReturn(CommonLispSymbols.NIL, code);
		}

		if (code instanceof NumberStruct) {
			return new BackquoteReturn(CommonLispSymbols.T, code);
		}

		if (code instanceof SymbolStruct<?>) {
			// Need to check the constant symbols here
			final SymbolStruct<?> codeSymbol = (SymbolStruct<?>) code;
			if (CommonLispSymbols.T.equals(codeSymbol)) {
				return new BackquoteReturn(CommonLispSymbols.T, code);
			} else if (CommonLispSymbols.NIL.equals(codeSymbol)) {
				return new BackquoteReturn(CommonLispSymbols.NIL, code);
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
					final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
					return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
				}
			}

			if (CommonLispSymbols.APPEND.equals(carConsCode)
					|| CommonLispSymbols.LIST.equals(carConsCode)
					|| CommonLispSymbols.NCONC.equals(carConsCode)) {

				final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
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
			final LispStruct flag = consStruct.getFirst();
			if (CommonLispSymbols.BQ_AT_FLAG.equals(flag) || CommonLispSymbols.BQ_DOT_FLAG.equals(flag)) {
				return true;
			}
		}
		return false;
	}

	private static LispStruct backquotify_1(final SymbolStruct<?> flag, final LispStruct thing) {

		if (CommonLispSymbols.BQ_COMMA_FLAG.equals(flag) || CommonLispSymbols.T.equals(flag) || CommonLispSymbols.NIL.equals(flag)) {
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

				if (NullStruct.INSTANCE.equals(cddrThing) && !expandableBackqExpressionP(cadrThing)) {
					// Basically if there are only 2 items in the list, just use Cons function
					return new ConsStruct(CommonLispSymbols.CONS, thing);
				}

				final ListStruct lastThing = consThing.getLast();
				final LispStruct carOfLastThing = lastThing.getFirst();

				if (expandableBackqExpressionP(carOfLastThing)) {
					final ListStruct allButLast = consThing.getAllButLast();
					final ConsStruct consStruct = new ConsStruct(CommonLispSymbols.LIST, allButLast);

					return ListStruct.buildProperList(CommonLispSymbols.APPEND, consStruct, carOfLastThing);
				}
			}

			return new ConsStruct(CommonLispSymbols.LIST_STAR, thing);
		}

		return new ConsStruct(flag, thing);
	}

	private static final class BackquoteReturn {

		private final SymbolStruct<?> flag;

		private final LispStruct thing;

		private BackquoteReturn(final SymbolStruct<?> flag, final LispStruct thing) {
			this.flag = flag;
			this.thing = thing;
		}

		public SymbolStruct<?> getFlag() {
			return flag;
		}

		public LispStruct getThing() {
			return thing;
		}

		@Override
		public int hashCode() {
			return HashCodeBuilder.reflectionHashCode(this);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj);
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
