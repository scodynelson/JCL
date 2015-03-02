/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.NumberElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.system.EnhancedLinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '`' Lisp reader macro.
 */
@Component
public class BackquoteReaderMacroFunction extends BackquoteFacilityMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7900660772057166319L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BackquoteReaderMacroFunction.class);

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


CMUCL:
(LIST (QUOTE COND) (CONS (LIST (QUOTE NUMBERP) X) Y) (LIST* (QUOTE T) (LIST (QUOTE PRINT) X) Y))
(LIST* (QUOTE X) X (APPEND X (LIST* (QUOTE FOO) (CADR X) (QUOTE BAR) (CDR X) (QUOTE BAZ) (CDR X))))
(LIST* (CONS A (QUOTE (B))) C D)

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
                     (APPEND (SYSTEM::%MAPCAN
                              #'(LAMBDA (X) (AND X (LIST X)))
                              (BUILD-LAMBDA-ELEMENT (FIRST PARSED-LAMBDA-LIST) COUNT ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM))
                             RESULT))
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
                   (FIRST-PART-OF-PARSER
                    (SYSTEM::%MAPCAR #'(LAMBDA (X) (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X))))) PAIRED-GENSYM-REQ-OPT))
                   (SECOND-PART-OF-PARSER (IF FAKE-NEEDED (INTERN (SYMBOL-NAME (GENSYM '|FakeRest-|))) (NTH (LENGTH PAIRED-GENSYM-REQ-OPT) GENSYM-ARG-LIST)))
                   (FOURTH-PART-OF-PARSER
                    (SYSTEM::%MAPCAR
                     #'(LAMBDA (X)
                         (LET ((SYS-%GET-FORM (CADADR X)))
                           (WHEN (EQ (FIRST SYS-%GET-FORM) 'SYSTEM::%GET-PLIST) (RPLACA (REST SYS-%GET-FORM) SECOND-PART-OF-PARSER)))
                         (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X)))))
                     (PAIRUP
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) GENSYM-ARG-LIST)
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) (NTHCDR 0 RESULT))
                      (- RESULT-LENGTH REQ-AND-OPT-COUNT)))))
              (LET ((OUTPUT
                     (LIST 'LIST
                           ''LET*
                           (CONS 'APPEND
                                 (APPEND FIRST-PART-OF-PARSER
                                         (LIST (LIST 'CONS
                                                     (LIST 'LIST
                                                           (LIST 'QUOTE SECOND-PART-OF-PARSER)
                                                           (LIST 'CONS ''LIST (LIST 'NTHCDR ALTERED-REQ-OPT-P-COUNT ARGLIST-PARAM-SYM)))
                                                     (CONS 'APPEND FOURTH-PART-OF-PARSER)))))
                           (LIST 'LIST* ''%FUNCTION-MARKER% FN (LIST 'QUOTE GENSYM-ARG-LIST)))))
                (SETQ *REST-ARGS-PARAM* NIL)
                OUTPUT))))
      (LIST* 'LIST ''%FUNCTION-MARKER% FN RESULT))))


JCL:
(LIST (QUOTE COND) (CONS (LIST (QUOTE NUMBERP) X) Y) (LIST* (QUOTE T) (LIST (QUOTE PRINT) X) Y))
(LIST* (QUOTE X) X (APPEND X (LIST* (QUOTE FOO) (CADR X) (QUOTE BAR) (CDR X) (QUOTE BAZ) (CDR X))))
(LIST* (CONS A (QUOTE (B))) C D)

++ Original
(DEFUN BUILD-LAMBDA-ELEMENTS (PARSED-LAMBDA-LIST ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM FN)
  (DECLARE (%JAVA-CLASS-NAME "lisp.system.compiler.BuildLambdaLists"))
  (SETQ PARSED-LAMBDA-LIST (KILL-FAKE-ENTRY PARSED-LAMBDA-LIST))
  (LET ((RESULT NIL)
        (COUNT 0)
        (OPTIONAL-P-COUNT (CDR (COUNT-PARAMETERS PARSED-LAMBDA-LIST OPTIONAL)))
        (KEY-P-COUNT (CDR (COUNT-PARAMETERS PARSED-LAMBDA-LIST KEY)))
        (LET-VARIABLE (GENSYM "LetVariable-"))
        (REST-OR-KEY (REST-OR-KEY-PARAM-P PARSED-LAMBDA-LIST)))
    (SETQ *REST-ARGS-PARAM*
          (LET ((PARAM-TYPE REST-OR-KEY))
            (IF PARAM-TYPE (LIST (GET-NAME (SECOND PARAM-TYPE)) (COUNT-REQ-AND-OPT-PARAMS PARSED-LAMBDA-LIST (FIRST PARAM-TYPE))) NIL)))
    (TAGBODY
     TOP     (WHEN PARSED-LAMBDA-LIST
               (SETQ RESULT
                     (APPEND (%MAPCAN
                              #'(LAMBDA (X) (AND X (LIST X)))
                              (BUILD-LAMBDA-ELEMENT (FIRST PARSED-LAMBDA-LIST) COUNT ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM))
                             RESULT))
               (SETQ PARSED-LAMBDA-LIST (REST PARSED-LAMBDA-LIST))
               (UNLESS (EQ (GET-USAGE (FIRST PARSED-LAMBDA-LIST)) SUPPLIED-P) (SETQ COUNT (1+ COUNT)))
               (GO TOP)))
    (SETQ RESULT (%REVERSE RESULT))
    (IF *REST-ARGS-PARAM*
        (LET ((&REST-NAME (FIRST *REST-ARGS-PARAM*)) (REQ-AND-OPT-COUNT (SECOND *REST-ARGS-PARAM*)))
          (LET* ((ALTERED-REQ-OPT-P-COUNT (- REQ-AND-OPT-COUNT OPTIONAL-P-COUNT))
                 (FAKE-NEEDED (EQ (FIRST REST-OR-KEY) KEY))
                 (RESULT-LENGTH (IF FAKE-NEEDED (LENGTH RESULT) (1+ (LENGTH RESULT))))
                 (GENSYM-ARG-LIST (MAKE-GENSYM-LIST (IF FAKE-NEEDED RESULT-LENGTH (1- RESULT-LENGTH)))))
            (UNLESS FAKE-NEEDED
              PUTS
              IN
              THE
              REAL
              &REST
              PARAM
              NAME
              IF
              NEEDED
              (SETQ &REST-NAME (NTH (- REQ-AND-OPT-COUNT OPTIONAL-P-COUNT) GENSYM-ARG-LIST))
              (SETQ RESULT (CHANGE-VARIABLE-NAME RESULT &REST-NAME)))
            (LET* ((PAIRED-GENSYM-REQ-OPT (PAIRUP GENSYM-ARG-LIST RESULT REQ-AND-OPT-COUNT))
                   (FIRST-PART-OF-PARSER
                    (%MAPCAR #'(LAMBDA (X) (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X))))) PAIRED-GENSYM-REQ-OPT))
                   (SECOND-PART-OF-PARSER (IF FAKE-NEEDED (INTERN (SYMBOL-NAME (GENSYM "FakeRest-"))) (NTH (LENGTH PAIRED-GENSYM-REQ-OPT) GENSYM-ARG-LIST)))
                   (FOURTH-PART-OF-PARSER
                    (%MAPCAR
                     #'(LAMBDA (X)
                         (LET ((SYS-%GET-FORM (CADADR X))) (WHEN (EQ (FIRST SYS-%GET-FORM) '%GET-PLIST) (RPLACA (REST SYS-%GET-FORM) SECOND-PART-OF-PARSER)))
                         (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X)))))
                     (PAIRUP
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) GENSYM-ARG-LIST)
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) (NTHCDR 0 RESULT))
                      (- RESULT-LENGTH REQ-AND-OPT-COUNT)))))
              (LET ((OUTPUT
                     (LIST 'LIST
                           ''LET*
                           (CONS 'APPEND
                                 (APPEND FIRST-PART-OF-PARSER
                                         (LIST (LIST 'CONS
                                                     (LIST 'LIST
                                                           (LIST 'QUOTE SECOND-PART-OF-PARSER)
                                                           (LIST 'CONS ''LIST (LIST 'NTHCDR ALTERED-REQ-OPT-P-COUNT ARGLIST-PARAM-SYM)))
                                                     (CONS 'APPEND FOURTH-PART-OF-PARSER)))))
                           (LIST 'LIST* ''%FUNCTION-MARKER% FN (LIST 'QUOTE GENSYM-ARG-LIST)))))
                (SETQ *REST-ARGS-PARAM* NIL)
                *
                CLEAR
                THE
                SPECIAL
                VARIABLE
                OUTPUT))))
      (LIST* 'LIST ''%FUNCTION-MARKER% FN RESULT))))

++ Cleaned
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
                     (APPEND (SYSTEM::%MAPCAN
                              #'(LAMBDA (X) (AND X (LIST X)))
                              (BUILD-LAMBDA-ELEMENT (FIRST PARSED-LAMBDA-LIST) COUNT ARGLIST-PARAM-SYM ARGLIST-LENGTH-SYM))
                             RESULT))
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
                   (FIRST-PART-OF-PARSER
                    (SYSTEM::%MAPCAR #'(LAMBDA (X) (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X))))) PAIRED-GENSYM-REQ-OPT))
                   (SECOND-PART-OF-PARSER (IF FAKE-NEEDED (INTERN (SYMBOL-NAME (GENSYM '|FakeRest-|))) (NTH (LENGTH PAIRED-GENSYM-REQ-OPT) GENSYM-ARG-LIST)))
                   (FOURTH-PART-OF-PARSER
                    (SYSTEM::%MAPCAR
                     #'(LAMBDA (X)
                         (LET ((SYS-%GET-FORM (CADADR X)))
                           (WHEN (EQ (FIRST SYS-%GET-FORM) 'SYSTEM::%GET-PLIST) (RPLACA (REST SYS-%GET-FORM) SECOND-PART-OF-PARSER)))
                         (LIST 'IDENTITY (LIST 'LIST (LIST* 'LIST (LIST 'QUOTE (FIRST X)) (REST X)))))
                     (PAIRUP
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) GENSYM-ARG-LIST)
                      (NTHCDR (IF FAKE-NEEDED REQ-AND-OPT-COUNT (1+ REQ-AND-OPT-COUNT)) (NTHCDR 0 RESULT))
                      (- RESULT-LENGTH REQ-AND-OPT-COUNT)))))
              (LET ((OUTPUT
                     (LIST 'LIST
                           ''LET*
                           (CONS 'APPEND
                                 (APPEND FIRST-PART-OF-PARSER
                                         (LIST (LIST 'CONS
                                                     (LIST 'LIST
                                                           (LIST 'QUOTE SECOND-PART-OF-PARSER)
                                                           (LIST 'CONS ''LIST (LIST 'NTHCDR ALTERED-REQ-OPT-P-COUNT ARGLIST-PARAM-SYM)))
                                                     (CONS 'APPEND FOURTH-PART-OF-PARSER)))))
                           (LIST 'LIST* ''%FUNCTION-MARKER% FN (LIST 'QUOTE GENSYM-ARG-LIST)))))
                (SETQ *REST-ARGS-PARAM* NIL)
                OUTPUT))))
      (LIST* 'LIST ''%FUNCTION-MARKER% FN RESULT))))
	 */

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.GRAVE_ACCENT, this, false);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		reader.increaseBackquoteLevel();
		try {
			final SimpleElement code = reader.read();
			final BackquoteReturn backquoteReturn = backquotify(code);

			final SymbolElement flag = backquoteReturn.getFlag();
			final SimpleElement thing = backquoteReturn.getThing();

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

	private BackquoteReturn backquotify(final SimpleElement code) {

		if (!(code instanceof ConsElement)) {
			if (code instanceof NullElement) {
				return new BackquoteReturn(NIL, NullElement.INSTANCE);
			}

			if (code instanceof SymbolElement) {
				return new BackquoteReturn(QUOTE, code);
			}

			return new BackquoteReturn(T, code);
		}

		final ConsElement consCode = (ConsElement) code;
		final EnhancedLinkedList<SimpleElement> consCodeElements = consCode.getElements();

		final SimpleElement carConsCode = consCodeElements.getFirst();
		final EnhancedLinkedList<SimpleElement> cdrConsCode = consCodeElements.getAllButFirst();

		final SimpleElement cdrElement = getCdrElement(cdrConsCode, consCode.isDotted());

		if (BQ_AT_FLAG.equals(carConsCode) || BQ_DOT_FLAG.equals(carConsCode)) {
			final SymbolElement carConsCodeFlag = (SymbolElement) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrElement);
		}

		if (BQ_COMMA_FLAG.equals(carConsCode)) {
			return comma(cdrElement);
		}

		final BackquoteReturn carBqtify = backquotify(carConsCode);

		final SymbolElement carBqtifyFlag = carBqtify.getFlag();
		SimpleElement carBqtifyThing = carBqtify.getThing();

		final BackquoteReturn cdrBqtify = backquotify(cdrElement);

		final SymbolElement cdrBqtifyFlag = cdrBqtify.getFlag();
		final SimpleElement cdrBqtifyThing = cdrBqtify.getThing();

		if (BQ_AT_FLAG.equals(cdrBqtifyFlag)) {
			throw new ReaderErrorException(",@ after dot in " + printer.print(code));
		}

		if (BQ_DOT_FLAG.equals(cdrBqtifyFlag)) {
			throw new ReaderErrorException(",. after dot in " + printer.print(code));
		}

		if (BQ_AT_FLAG.equals(carBqtifyFlag)) {
			if (NIL.equals(cdrBqtifyFlag)) {
				if (expandableBackqExpressionP(carBqtifyThing)) {
					return new BackquoteReturn(APPEND, new ConsElement(carBqtifyThing));
				} else {
					return comma(carBqtifyThing);
				}
			} else {

				final SimpleElement bqReturnThing;
				if (APPEND.equals(cdrBqtifyFlag)) {
					bqReturnThing = getConsElement(carBqtifyThing, cdrBqtifyThing);
				} else {
					final SimpleElement backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = new ConsElement(carBqtifyThing, backquotify_1);
				}

				return new BackquoteReturn(APPEND, bqReturnThing);
			}
		}

		if (BQ_DOT_FLAG.equals(carBqtifyFlag)) {
			if (NIL.equals(cdrBqtifyFlag)) {
				if (expandableBackqExpressionP(carBqtifyThing)) {
					return new BackquoteReturn(NCONC, new ConsElement(carBqtifyThing));
				} else {
					return comma(carBqtifyThing);
				}
			} else {

				final SimpleElement bqReturnThing;
				if (NCONC.equals(cdrBqtifyFlag)) {
					bqReturnThing = getConsElement(carBqtifyThing, cdrBqtifyThing);
				} else {
					final SimpleElement backquotify_1 = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = new ConsElement(carBqtifyThing, backquotify_1);
				}

				return new BackquoteReturn(NCONC, bqReturnThing);
			}
		}

		if (NIL.equals(cdrBqtifyFlag)) {
			if (QUOTE.equals(carBqtifyFlag) || T.equals(carBqtifyFlag) || NIL.equals(carBqtifyFlag)) {
				return new BackquoteReturn(QUOTE, new ConsElement(carBqtifyThing));
			} else {

				final SimpleElement backquotify_1 = backquotify_1(carBqtifyFlag, carBqtifyThing);
				final SimpleElement bqReturnThing = new ConsElement(backquotify_1);

				return new BackquoteReturn(LIST, bqReturnThing);
			}
		}

		if (QUOTE.equals(cdrBqtifyFlag) || T.equals(cdrBqtifyFlag)) {
			if (QUOTE.equals(carBqtifyFlag) || T.equals(carBqtifyFlag) || NIL.equals(carBqtifyFlag)) {
				final ConsElement bqReturnThing = getConsElement(carBqtifyThing, cdrBqtifyThing);
				return new BackquoteReturn(QUOTE, bqReturnThing);
			} else {

				final SimpleElement backquotify_1_a = backquotify_1(carBqtifyFlag, carBqtifyThing);
				final SimpleElement backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
				// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
				final ConsElement bqReturnThing = new ConsElement(backquotify_1_a, backquotify_1_d);

				return new BackquoteReturn(LIST_STAR, bqReturnThing);
			}
		}

		carBqtifyThing = backquotify_1(carBqtifyFlag, carBqtifyThing);

		if (LIST.equals(cdrBqtifyFlag) || LIST_STAR.equals(cdrBqtifyFlag)) {
			final ConsElement bqReturnThing = getConsElement(carBqtifyThing, cdrBqtifyThing);
			return new BackquoteReturn(cdrBqtifyFlag, bqReturnThing);
		}

		final SimpleElement backquotify_1_d = backquotify_1(cdrBqtifyFlag, cdrBqtifyThing);
		// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
		final ConsElement bqReturnThing = new ConsElement(carBqtifyThing, backquotify_1_d);

		return new BackquoteReturn(LIST_STAR, bqReturnThing);
	}

	private BackquoteReturn comma(final SimpleElement code) {

		if (!(code instanceof ConsElement)) {
			if (code instanceof NullElement) {
				return new BackquoteReturn(NIL, NullElement.INSTANCE);
			}

			if (code instanceof NumberElement) {
				return new BackquoteReturn(T, code);
			}

			if (code instanceof SymbolElement) {
				final SymbolElement codeSymbol = (SymbolElement) code;
				if (T.equals(codeSymbol)) {
					return new BackquoteReturn(T, code);
				}
			}

			return new BackquoteReturn(BQ_COMMA_FLAG, code);
		}

		final ConsElement consCode = (ConsElement) code;
		final EnhancedLinkedList<SimpleElement> consCodeElements = consCode.getElements();

		final SimpleElement carConsCode = consCodeElements.getFirst();
		final EnhancedLinkedList<SimpleElement> cdrConsCode = consCodeElements.getAllButFirst();

		final SimpleElement cadrConsCode = cdrConsCode.getFirst();

		if (QUOTE.equals(carConsCode) && !expandableBackqExpressionP(cadrConsCode)) {
			final SymbolElement carConsCodeFlag = (SymbolElement) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
		}

		final SimpleElement cdrElement = getCdrElement(cdrConsCode, consCode.isDotted());

		if (APPEND.equals(carConsCode) || LIST.equals(carConsCode) || NCONC.equals(carConsCode)) {
			final SymbolElement carConsCodeFlag = (SymbolElement) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrElement);
		}

		if (CONS.equals(carConsCode) || LIST_STAR.equals(carConsCode)) {
			return new BackquoteReturn(LIST_STAR, cdrElement);
		}

		return new BackquoteReturn(BQ_COMMA_FLAG, code);
	}

	private boolean expandableBackqExpressionP(final SimpleElement o) {
		if (o instanceof ConsElement) {
			final ConsElement consElement = (ConsElement) o;
			final SimpleElement flag = consElement.getElements().getFirst();
			if (BQ_AT_FLAG.equals(flag) || BQ_DOT_FLAG.equals(flag)) {
				return true;
			}
		}
		return false;
	}

	private SimpleElement backquotify_1(final SymbolElement flag, final SimpleElement thing) {

		if (BQ_COMMA_FLAG.equals(flag) || T.equals(flag) || NIL.equals(flag)) {
			return thing;
		}

		if (QUOTE.equals(flag)) {
			// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
			return new ConsElement(QUOTE, thing);
		}

		if (LIST_STAR.equals(flag)) {
			if (thing instanceof ListElement) {
				final ListElement listElementThing = (ListElement) thing;
				final boolean isDotted = listElementThing.isDotted();

				final EnhancedLinkedList<SimpleElement> listElements = listElementThing.getElements();

				final EnhancedLinkedList<SimpleElement> cdrThing = listElements.getAllButFirst();

				final SimpleElement cadrThing = cdrThing.getFirst();
				final EnhancedLinkedList<SimpleElement> cddrThing = cdrThing.getAllButFirst();

				if (cddrThing.isEmpty() && !expandableBackqExpressionP(cadrThing)) {
					final EnhancedLinkedList<SimpleElement> newElements = new EnhancedLinkedList<>();
					newElements.add(CONS);
					newElements.addAll(listElements);
					return new ConsElement(isDotted, newElements);
				}

				// TODO: check to make sure this is always fine!!!
				final SimpleElement last = listElements.getLast();

				final ListElement lastThing;
				if (last instanceof ListElement) {
					lastThing = (ListElement) last;
				} else {
					lastThing = new ConsElement(last);
				}

				final SimpleElement carOfLastThing = lastThing.getElements().getFirst();
				if (expandableBackqExpressionP(carOfLastThing)) {

					// NOTE: no need to worry about dotted conses here. The butlast operation doesn't return the last element
					//       and a cons pair is considered the last element. therefore, the new cons created next will never
					//       be a dotted cons.
					final EnhancedLinkedList<SimpleElement> allButLast = listElements.getAllButLast();
					allButLast.addFirst(LIST);

					final ConsElement consElement = new ConsElement(allButLast);

					final EnhancedLinkedList<SimpleElement> returnConsElements = new EnhancedLinkedList<>();
					returnConsElements.add(APPEND);
					returnConsElements.add(consElement);
					returnConsElements.add(carOfLastThing);

					return new ConsElement(returnConsElements);
				}

				final EnhancedLinkedList<SimpleElement> newElements = new EnhancedLinkedList<>();
				newElements.add(LIST_STAR);
				newElements.addAll(listElements);
				return new ConsElement(isDotted, newElements);
			} else {
				return new ConsElement(true, LIST_STAR, thing);
			}
		}

		return getConsElement(flag, thing);
	}

	static class BackquoteReturn {

		private final SymbolElement flag;

		private final SimpleElement thing;

		BackquoteReturn(final SymbolElement flag, final SimpleElement thing) {
			this.flag = flag;
			this.thing = thing;
		}

		public SymbolElement getFlag() {
			return flag;
		}

		public SimpleElement getThing() {
			return thing;
		}
	}
}
