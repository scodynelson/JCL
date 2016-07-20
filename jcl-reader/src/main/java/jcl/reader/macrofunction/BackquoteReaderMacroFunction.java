/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.CharacterConstants;
import jcl.lang.ConsStruct;
import jcl.lang.GlobalPackageStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.NumberStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import org.springframework.stereotype.Component;

/**
 * Implements the '`' Lisp reader macro.
 */
@Component
public class BackquoteReaderMacroFunction extends ReaderMacroFunction {

	private static final SymbolStruct APPEND = GlobalPackageStruct.COMMON_LISP.intern("APPEND").getSymbol();
	private static final SymbolStruct CONS = GlobalPackageStruct.COMMON_LISP.intern("CONS").getSymbol();
	private static final SymbolStruct LIST = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();
	private static final SymbolStruct LIST_STAR = GlobalPackageStruct.COMMON_LISP.intern("LIST*").getSymbol();
	private static final SymbolStruct NCONC = GlobalPackageStruct.COMMON_LISP.intern("NCONC").getSymbol();

	private static final SymbolStruct LIST_TO_VECTOR = GlobalPackageStruct.SYSTEM.intern("LIST-TO-VECTOR").getSymbol();

	public static final SymbolStruct BQ_COMMA_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",").getSymbol();
	public static final SymbolStruct BQ_AT_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",@").getSymbol();
	public static final SymbolStruct BQ_DOT_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",.").getSymbol();
	public static final SymbolStruct BQ_VECTOR_FLAG = GlobalPackageStruct.BACKQUOTE.intern("bqv").getSymbol();

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

			if (BQ_AT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",@ after backquote in " + thing);
			}
			if (BQ_DOT_FLAG.equals(flag)) {
				throw new ReaderErrorException(",. after backquote in " + thing);
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
			return new BackquoteReturn(SpecialOperatorStruct.QUOTE, code);
		}

		if (code instanceof ConsStruct) {
			final ConsStruct consCode = (ConsStruct) code;

			final LispStruct carConsCode = consCode.getCar();
			final LispStruct cdrConsCode = consCode.getCdr();

			if (BQ_AT_FLAG.equals(carConsCode) || BQ_DOT_FLAG.equals(carConsCode)) {
				final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
				return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
			}

			if (BQ_COMMA_FLAG.equals(carConsCode)) {
				return comma(cdrConsCode);
			}

			if (BQ_VECTOR_FLAG.equals(carConsCode)) {
				final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

				final SymbolStruct cdrBqtifyFlag = cdrBqtify.getFlag();
				final LispStruct cdrBqtifyThing = cdrBqtify.getThing();

				final LispStruct cdrBqtify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				return new BackquoteReturn(LIST_TO_VECTOR, cdrBqtify_1);
			}

			final BackquoteReturn carBqtify = backquotify(carConsCode);

			final SymbolStruct carBqtifyFlag = carBqtify.getFlag();
			final LispStruct carBqtifyThing = carBqtify.getThing();

			final BackquoteReturn cdrBqtify = backquotify(cdrConsCode);

			final SymbolStruct cdrBqtifyFlag = cdrBqtify.getFlag();
			final LispStruct cdrBqtifyThing = cdrBqtify.getThing();

			if (BQ_AT_FLAG.equals(cdrBqtifyFlag)) {
				throw new ReaderErrorException(",@ after dot in " + code);
			}

			if (BQ_DOT_FLAG.equals(cdrBqtifyFlag)) {
				throw new ReaderErrorException(",. after dot in " + code);
			}

			if (BQ_AT_FLAG.equals(carBqtifyFlag)) {
				return backquotifyAtFlag(carBqtifyThing, cdrBqtifyFlag, cdrBqtifyThing);
			}

			if (BQ_DOT_FLAG.equals(carBqtifyFlag)) {
				return backquotifyDotFlag(carBqtifyThing, cdrBqtifyFlag, cdrBqtifyThing);
			}

			if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {

				if (SpecialOperatorStruct.QUOTE.equals(carBqtifyFlag)
						|| TStruct.INSTANCE.equals(carBqtifyFlag)
						|| NILStruct.INSTANCE.equals(carBqtifyFlag)) {

					final ListStruct bqReturnThing = ListStruct.buildProperList(carBqtifyThing);
					return new BackquoteReturn(SpecialOperatorStruct.QUOTE, bqReturnThing);
				} else {

					final LispStruct backquotify_1 = backquotify_1(carBqtifyFlag, carBqtifyThing);
					final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1);

					return new BackquoteReturn(LIST, bqReturnThing);
				}
			}

			if (SpecialOperatorStruct.QUOTE.equals(cdrBqtifyFlag)
					|| TStruct.INSTANCE.equals(cdrBqtifyFlag)) {

				if (SpecialOperatorStruct.QUOTE.equals(carBqtifyFlag)
						|| TStruct.INSTANCE.equals(carBqtifyFlag)
						|| NILStruct.INSTANCE.equals(carBqtifyFlag)) {

					final ConsStruct bqReturnThing = new ConsStruct(carBqtifyThing, cdrBqtifyThing);
					return new BackquoteReturn(SpecialOperatorStruct.QUOTE, bqReturnThing);
				} else {

					final LispStruct backquotify_1_a = backquotify_1(carBqtifyFlag, carBqtifyThing);
					final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					final ListStruct bqReturnThing = ListStruct.buildProperList(backquotify_1_a, backquotify_1_d);

					return new BackquoteReturn(LIST_STAR, bqReturnThing);
				}
			}

			final LispStruct nextCarBqtifyThing = backquotify_1(carBqtifyFlag, carBqtifyThing);

			if (LIST.equals(cdrBqtifyFlag)
					|| LIST_STAR.equals(cdrBqtifyFlag)) {
				final ListStruct bqReturnThing = new ConsStruct(nextCarBqtifyThing, cdrBqtifyThing);
				return new BackquoteReturn(cdrBqtifyFlag, bqReturnThing);
			}

			final LispStruct backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
			final ListStruct bqReturnThing = ListStruct.buildProperList(nextCarBqtifyThing, backquotify_1_d);

			return new BackquoteReturn(LIST_STAR, bqReturnThing);
		}

		return new BackquoteReturn(TStruct.INSTANCE, code);
	}

	private BackquoteReturn backquotifyAtFlag(final LispStruct carBqtifyThing, final SymbolStruct cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {
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
				bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
			}

			return new BackquoteReturn(APPEND, bqReturnThing);
		}
	}

	private BackquoteReturn backquotifyDotFlag(final LispStruct carBqtifyThing, final SymbolStruct cdrBqtifyFlag, final LispStruct cdrBqtifyThing) {
		if (NILStruct.INSTANCE.equals(cdrBqtifyFlag)) {
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
				bqReturnThing = ListStruct.buildProperList(carBqtifyThing, backquotify_1);
			}

			return new BackquoteReturn(NCONC, bqReturnThing);
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

			if (SpecialOperatorStruct.QUOTE.equals(carConsCode)) {
				// NOTE: This cast will always be fine because of how we build the ConsStruct in the CommaReaderMacroFunction
				final LispStruct cadrConsCode = ((ConsStruct) cdrConsCode).getCar();
				if (!expandableBackqExpressionP(cadrConsCode)) {
					final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
					return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
				}
			}

			if (APPEND.equals(carConsCode)
					|| LIST.equals(carConsCode)
					|| NCONC.equals(carConsCode)) {

				final SymbolStruct carConsCodeFlag = (SymbolStruct) carConsCode;
				return new BackquoteReturn(carConsCodeFlag, cdrConsCode);
			}

			if (CONS.equals(carConsCode)
					|| LIST_STAR.equals(carConsCode)) {

				return new BackquoteReturn(LIST_STAR, cdrConsCode);
			}
		}

		return new BackquoteReturn(BQ_COMMA_FLAG, code);
	}

	private static boolean expandableBackqExpressionP(final LispStruct o) {
		if (o instanceof ConsStruct) {
			final ConsStruct consStruct = (ConsStruct) o;
			final LispStruct flag = consStruct.getCar();
			if (BQ_AT_FLAG.equals(flag) || BQ_DOT_FLAG.equals(flag)) {
				return true;
			}
		}
		return false;
	}

	private static LispStruct backquotify_1(final SymbolStruct flag, final LispStruct thing) {

		if (BQ_COMMA_FLAG.equals(flag) || TStruct.INSTANCE.equals(flag) || NILStruct.INSTANCE.equals(flag)) {
			return thing;
		}

		if (SpecialOperatorStruct.QUOTE.equals(flag)) {
			return ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, thing);
		}

		if (LIST_STAR.equals(flag)) {
			// NOTE: The following check is not in CMU-CL. Actually a semi-bug found when handling improperly created lists.
			if (thing instanceof ConsStruct) {
				final ConsStruct consThing = (ConsStruct) thing;

				final LispStruct cdrThing = consThing.getCdr();

				// NOTE: This will always be an ok cast due to the backquote reader algorithm
				final LispStruct cadrThing = ((ConsStruct) cdrThing).getCar();
				final LispStruct cddrThing = ((ConsStruct) cdrThing).getCdr();

				if (NILStruct.INSTANCE.equals(cddrThing) && !expandableBackqExpressionP(cadrThing)) {
					// Basically if there are only 2 items in the list, just use Cons function
					return new ConsStruct(CONS, thing);
				}

				// NOTE: This is a safe cast, as the last() will always return a List with the last cons, even if the list is dotted
				final ListStruct lastThing = (ListStruct) consThing.last();
				final LispStruct carOfLastThing = lastThing.getCar();

				if (expandableBackqExpressionP(carOfLastThing)) {
					final ListStruct allButLast = consThing.butLast();
					final ConsStruct consStruct = new ConsStruct(LIST, allButLast);

					return ListStruct.buildProperList(APPEND, consStruct, carOfLastThing);
				}
			}

			return new ConsStruct(LIST_STAR, thing);
		}

		if (LIST_TO_VECTOR.equals(flag)) {
			return ListStruct.buildProperList(LIST_TO_VECTOR, thing);
		}

		return new ConsStruct(flag, thing);
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
	}
}