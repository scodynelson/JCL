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
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
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

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/*

ORIGINAL:
`(cond ((numberp ,x) ,@y) (t (print ,x) ,@y))
`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
`((,a b) ,c ,@d)

(DEFUN BUILD-LAMBDA-ELEMENTS (PARSED-LAMBDA-LIST ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM FN)
  (SETQ PARSED-LAMBDA-LIST (KILL-FAKE-ENTRY PARSED-LAMBDA-LIST))
  (LET ((RESULT NIL)
        (COUNT 0)
        (OPTIONAL-P-COUNT (CDR (COUNT-PARAMETERS PARSED-LAMBDA-LIST :OPTIONAL)))
        (KEY-P-COUNT (CDR (COUNT-PARAMETERS PARSED-LAMBDA-LIST :KEY)))
        (LET-VARIABLE (GENSYM '|LetVariable-|))
        (REST-OR-KEY (REST-OR-KEY-PARAM-P PARSED-LAMBDA-LIST)))
    (SETQ *REST-ARGS-PARAM*
          (LET ((PARAM-TYPE REST-OR-KEY))
            (IF PARAM-TYPE (LIST (GET-NAME (SECOND PARAM-TYPE)) (COUNT-REQ-AND-OPT-PARAMS PARSED-LAMBDA-LIST (FIRST PARAM-TYPE))) NIL)))
    (TAGBODY
     TOP     (WHEN PARSED-LAMBDA-LIST
               (SETQ RESULT
                     `(,@(SYSTEM::%MAPCAN
                          #'(LAMBDA (X) (AND X (LIST X)))
                          (BUILD-LAMBDA-ELEMENT (FIRST PARSED-LAMBDA-LIST) COUNT ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM))
                       ,@RESULT))
               (SETQ PARSED-LAMBDA-LIST (REST PARSED-LAMBDA-LIST))
               (UNLESS (EQ (GET-USAGE (FIRST PARSED-LAMBDA-LIST)) :SUPPLIED-P) (SETQ COUNT (1+ COUNT)))
               (GO TOP)))
    (SETQ RESULT (SYSTEM::%REVERSE RESULT))
    (IF *REST-ARGS-PARAM*
        (LET ((&REST-NAME (FIRST *REST-ARGS-PARAM*)) (REQ-AND-OPT-COUNT (SECOND *REST-ARGS-PARAM*)))
          (LET* ((ALTERED-REQ-OPT-P-COUNT (- REQ-AND-OPT-COUNT OPTIONAL-P-COUNT))
                 (FAKE-NEEDED (EQ (FIRST REST-OR-KEY) :KEY))
                 (RESULT-LENGTH (IF FAKE-NEEDED (LENGTH RESULT) (1+ (LENGTH RESULT))))
                 (GENSYM-ARG-LIST (MAKE-GENSYM-LIST (IF FAKE-NEEDED RESULT-LENGTH (1- RESULT-LENGTH)))))
            (UNLESS FAKE-NEEDED
              (SETQ &REST-NAME (NTH (- REQ-AND-OPT-COUNT OPTIONAL-P-COUNT) GENSYM-ARG-LIST))
              (SETQ RESULT (CHANGE-VARIABLE-NAME RESULT &REST-NAME)))
            (LET* ((PAIRED-GENSYM-REQ-OPT (PAIRUP GENSYM-ARG-LIST RESULT REQ-AND-OPT-COUNT))
                   (FIRST-PART-OF-PARSER (SYSTEM::%MAPCAR #'(LAMBDA (X) `(IDENTITY (LIST (LIST ',(FIRST X) ,@(REST X))))) PAIRED-GENSYM-REQ-OPT))
                   (SECOND-PART-OF-PARSER (IF FAKE-NEEDED (INTERN (SYMBOL-NAME (GENSYM '|FakeRest-|))) (NTH (LENGTH PAIRED-GENSYM-REQ-OPT) GENSYM-ARG-LIST)))
                   (FOURTH-PART-OF-PARSER
                    (SYSTEM::%MAPCAR
                     #'(LAMBDA (X)
                         (LET ((SYS-%GET-FORM (CADADR X)))
                           (WHEN (EQ (FIRST SYS-%GET-FORM) 'SYSTEM::%GET-PLIST) (RPLACA (REST SYS-%GET-FORM) SECOND-PART-OF-PARSER)))
                         `(IDENTITY (LIST (LIST ',(FIRST X) ,@(REST X)))))
                     (PAIRUP
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) GENSYM-ARG-LIST)
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) (NTHCDR 0 RESULT))
                      (- RESULT-LENGTH REQ-AND-OPT-COUNT)))))
              (LET ((OUTPUT
                     `(LIST 'LET*
                            `(,@,@FIRST-PART-OF-PARSER
                              (,',SECOND-PART-OF-PARSER (LIST ,@(NTHCDR ,ALTERED-REQ-OPT-P-COUNT ,ARGLIST-PARAM-SYM)))
                              ,@,@FOURTH-PART-OF-PARSER)
                            `(COMPILER::%FUNCTION-MARKER% ,,FN ,@',GENSYM-ARG-LIST))))
                (SETQ *REST-ARGS-PARAM* NIL)
                OUTPUT))))
      `(LIST 'COMPILER::%FUNCTION-MARKER% ,FN ,@RESULT))))

ORIGINAL:
`(cond ((numberp ,x) ,@y) (t (print ,x) ,@y))
`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
`((,a b) ,c ,@d)


CMUCL:
(LIST (QUOTE COND) (CONS (LIST (QUOTE NUMBERP) X) Y) (LIST* (QUOTE T) (LIST (QUOTE PRINT) X) Y))
(LIST* (QUOTE X) X (APPEND X (LIST* (QUOTE FOO) (CADR X) (QUOTE BAR) (CDR X) (QUOTE BAZ) (CDR X))))
(LIST* (CONS A (QUOTE (B))) C D)


JCL:
(LIST (QUOTE COND) (CONS (LIST (QUOTE NUMBERP) X) Y) (LIST* (QUOTE T) (LIST (QUOTE PRINT) X) Y))
(LIST* (QUOTE X) X (APPEND X (LIST* (QUOTE FOO) (CADR X) (QUOTE BAR) (CDR X) (QUOTE BAZ) (CDR X))))
(LIST* (CONS A (QUOTE (B))) C D)
	 */

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

		reader.increaseBackquoteLevel();
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
			reader.decreaseBackquoteLevel();
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
