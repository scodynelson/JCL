/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '`' Lisp reader macro.
 */
@Component
public class BackquoteReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7900660772057166319L;

	static final SymbolStruct<?> APPEND = CommonLispSymbols.APPEND;

	static final SymbolStruct<?> CONS = CommonLispSymbols.CONS;

	static final SymbolStruct<?> LIST = CommonLispSymbols.LIST;

	static final SymbolStruct<?> LIST_STAR = CommonLispSymbols.LIST_STAR;

	static final SymbolStruct<?> NCONC = CommonLispSymbols.NCONC;

	static final SymbolStruct<?> NIL = CommonLispSymbols.NIL;

	static final SymbolStruct<?> QUOTE = CommonLispSymbols.QUOTE;

	static final SymbolStruct<?> T = CommonLispSymbols.T;

	static final SymbolStruct<?> BQ_COMMA_FLAG = new SymbolStruct<>(",", GlobalPackageStruct.BACKQUOTE);

	static final SymbolStruct<?> BQ_AT_FLAG = new SymbolStruct<>(",@", GlobalPackageStruct.BACKQUOTE);

	static final SymbolStruct<?> BQ_DOT_FLAG = new SymbolStruct<>(",.", GlobalPackageStruct.BACKQUOTE);

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
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		reader.incrementBackquoteLevel();
		try {
			final LispStruct code = reader.read(true, NullStruct.INSTANCE, true);
			final BackquoteReturn backquoteReturn = backquotify(code);

			final SymbolStruct<?> flag = backquoteReturn.getFlag();
			final LispStruct thing = backquoteReturn.getThing();

			if (BQ_AT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",@ after backquote in " + printer.print(thing));
			}
			if (BQ_DOT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",. after backquote in " + printer.print(thing));
			}

			return backquotify_1(flag, thing);
		} finally {
			reader.decrementBackquoteLevel();
		}
	}

	private BackquoteReturn backquotify(final LispStruct code) {

		if (!(code instanceof ConsStruct)) {
			if (code instanceof NullStruct) {
				return new BackquoteReturn(NIL, NullStruct.INSTANCE);
			}

			if (code instanceof SymbolStruct<?>) {
				return new BackquoteReturn(QUOTE, code);
			}

			return new BackquoteReturn(T, code);
		}

		final ConsStruct consCode = (ConsStruct) code;

		final LispStruct carConsCode = consCode.getCar();
		final LispStruct cdrConsCode = consCode.getCdr();

		if (BQ_AT_FLAG.equals(carConsCode) || BQ_DOT_FLAG.equals(carConsCode)) {
			final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
		}

		if (BQ_COMMA_FLAG.equals(carConsCode)) {
			return comma(cdrConsCode);
		}

		final BackquoteReturn carBqtify = backquotify(carConsCode);

		final SymbolStruct<?> carBqtifyFlag = carBqtify.getFlag();
		LispStruct carBqtifyThing = carBqtify.getThing();

		final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

		final SymbolStruct<?> cdrBqtifyFlag = cdrBqtify.getFlag();
		final LispStruct cdrBqtifyThing = cdrBqtify.getThing();

		if (BQ_AT_FLAG.equals(cdrBqtifyFlag)) {
			throw new ReaderErrorException(",@ after dot in " + printer.print(code));
		}

		if (BQ_DOT_FLAG.equals(cdrBqtifyFlag)) {
			throw new ReaderErrorException(",. after dot in " + printer.print(code));
		}

		if (BQ_AT_FLAG.equals(carBqtifyFlag)) {
			if (NIL.equals(cdrBqtifyFlag)) {
				if (expandableBackqExpressionP(carBqtifyThing)) {
					final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
					return new BackquoteReturn(APPEND, bqReturnThing);
				} else {
					return comma(carBqtifyThing);
				}
			} else {

				final ListStruct bqReturnThing;
				if (APPEND.equals(cdrBqtifyFlag)) {
					bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
				} else {
					final LispStruct backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
				}

				return new BackquoteReturn(APPEND, bqReturnThing);
			}
		}

		if (BQ_DOT_FLAG.equals(carBqtifyFlag)) {
			if (NIL.equals(cdrBqtifyFlag)) {
				if (expandableBackqExpressionP(carBqtifyThing)) {
					final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
					return new BackquoteReturn(NCONC, bqReturnThing);
				} else {
					return comma(carBqtifyThing);
				}
			} else {

				final ListStruct bqReturnThing;
				if (NCONC.equals(cdrBqtifyFlag)) {
					bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
				} else {
					final LispStruct backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
				}

				return new BackquoteReturn(NCONC, bqReturnThing);
			}
		}

		if (NIL.equals(cdrBqtifyFlag)) {
			if (QUOTE.equals(carBqtifyFlag) || T.equals(carBqtifyFlag) || NIL.equals(carBqtifyFlag)) {
				final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
				return new BackquoteReturn(QUOTE, bqReturnThing);
			} else {

				final LispStruct backquotify_1 = backquotify_1(carBqtifyFlag, carBqtifyThing);
				final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1);

				return new BackquoteReturn(LIST, bqReturnThing);
			}
		}

		if (QUOTE.equals(cdrBqtifyFlag) || T.equals(cdrBqtifyFlag)) {
			if (QUOTE.equals(carBqtifyFlag) || T.equals(carBqtifyFlag) || NIL.equals(carBqtifyFlag)) {
				final ConsStruct bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
				return new BackquoteReturn(QUOTE, bqReturnThing);
			} else {

				final LispStruct backquotify_1_a = backquotify_1(carBqtifyFlag, carBqtifyThing);
				final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
				final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1_a, backquotify_1_d);

				return new BackquoteReturn(LIST_STAR, bqReturnThing);
			}
		}

		carBqtifyThing = backquotify_1(carBqtifyFlag, carBqtifyThing);

		if (LIST.equals(cdrBqtifyFlag) || LIST_STAR.equals(cdrBqtifyFlag)) {
			final ListStruct bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
			return new BackquoteReturn(cdrBqtifyFlag, bqReturnThing);
		}

		final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
		// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
		final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1_d);

		return new BackquoteReturn(LIST_STAR, bqReturnThing);
	}

	private BackquoteReturn comma(final LispStruct code) {

		if (!(code instanceof ConsStruct)) {
			if (code instanceof NullStruct) {
				return new BackquoteReturn(NIL, NullStruct.INSTANCE);
			}

			if (code instanceof NumberStruct) {
				return new BackquoteReturn(T, code);
			}

			if (code instanceof SymbolStruct<?>) {
				final SymbolStruct<?> codeSymbol = (SymbolStruct<?>) code;
				if (T.equals(codeSymbol)) {
					return new BackquoteReturn(T, code);
				}
			}

			return new BackquoteReturn(BQ_COMMA_FLAG, code);
		}

		final ConsStruct consCode = (ConsStruct) code;

		final LispStruct carConsCode = consCode.getCar();
		final LispStruct cdrConsCode = consCode.getCdr();

		// TODO: check this!!!
		final LispStruct cadrConsCode = ((ConsStruct) cdrConsCode).getCar();

		if (QUOTE.equals(carConsCode) && !expandableBackqExpressionP(cadrConsCode)) {
			final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
		}

		if (APPEND.equals(carConsCode) || LIST.equals(carConsCode) || NCONC.equals(carConsCode)) {
			final SymbolStruct<?> carConsCodeFlag = (SymbolStruct<?>) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
		}

		if (CONS.equals(carConsCode) || LIST_STAR.equals(carConsCode)) {
			return new BackquoteReturn(LIST_STAR, cdrConsCode);
		}

		return new BackquoteReturn(BQ_COMMA_FLAG, code);
	}

	private static boolean expandableBackqExpressionP(final LispStruct o) {
		if (o instanceof ConsStruct) {
			final ConsStruct consStruct = (ConsStruct) o;
			final LispStruct flag = consStruct.getFirst();
			if (BQ_AT_FLAG.equals(flag) || BQ_DOT_FLAG.equals(flag)) {
				return true;
			}
		}
		return false;
	}

	private LispStruct backquotify_1(final SymbolStruct<?> flag, final LispStruct thing) {

		if (BQ_COMMA_FLAG.equals(flag) || T.equals(flag) || NIL.equals(flag)) {
			return thing;
		}

		if (QUOTE.equals(flag)) {
			// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
			return ListStruct.buildProperList(QUOTE, thing);
		}

		if (LIST_STAR.equals(flag)) {
			// NOTE: The following check is not in CMUCL. Actually a semi-bug found when handling improperly created lists.
			if (thing instanceof ConsStruct) {
				final ConsStruct consThing = (ConsStruct) thing;

				final LispStruct cdrThing = consThing.getCdr();

				// TODO: check this!!!
				final LispStruct cadrThing = ((ConsStruct) cdrThing).getCar();
				final LispStruct cddrThing = ((ConsStruct) cdrThing).getCdr();

				if (NullStruct.INSTANCE.equals(cddrThing) && !expandableBackqExpressionP(cadrThing)) {
					return new ConsStruct(CONS, thing);
				}

				final List<LispStruct> consThingElements = consThing.getAsJavaList();
				final EnhancedLinkedList<LispStruct> consElements = new EnhancedLinkedList<>(consThingElements);

				final ListStruct lastThing;

				final boolean isDotted = consThing.isDotted();
				if (isDotted) {
					final List<LispStruct> lastTwo = consElements.getLastN(2);
					lastThing = ListStruct.buildDottedList(lastTwo);
				} else {
					final LispStruct last = consElements.getLast();
					lastThing = ListStruct.buildProperList(last);
				}

				final LispStruct carOfLastThing = lastThing.getFirst();
				if (expandableBackqExpressionP(carOfLastThing)) {

					final EnhancedLinkedList<LispStruct> allButLast;
					if (isDotted) {
						allButLast = consElements.getAllButLastN(2);
					} else {
						allButLast = consElements.getAllButLast();
					}

					final ConsStruct consStruct = new ConsStruct(LIST, ListStruct.buildProperList(allButLast));
					return ListStruct.buildProperList(APPEND, consStruct, carOfLastThing);
				}

				return new ConsStruct(LIST_STAR, thing);
			} else {
				return new ConsStruct(LIST_STAR, thing);
			}
		}

		return new ConsStruct(flag, thing);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	static class BackquoteReturn {

		private final SymbolStruct<?> flag;

		private final LispStruct thing;

		BackquoteReturn(final SymbolStruct<?> flag, final LispStruct thing) {
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
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
