/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.NumberElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.system.CommonLispSymbols;
import jcl.system.EnhancedLinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.EnumMap;
import java.util.Map;

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
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BackquoteReaderMacroFunction.class);

	private static final SymbolElement APPEND = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "APPEND");

	private static final SymbolElement CONS = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "CONS");

	private static final SymbolElement LIST = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST");

	private static final SymbolElement LIST_STAR = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST*");

	private static final SymbolElement NCONC = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "NCONC");

	private static final SymbolElement QUOTE = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "QUOTE");

	static int backQuoteCount;

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

		backQuoteCount++;
		try {
			final SimpleElement code = reader.read();
			final BackquoteReturn backquoteReturn = backquotify(code);

			final Flag flag = backquoteReturn.getFlag();
			final SimpleElement thing = backquoteReturn.getThing();

			if (flag == Flag.BQ_AT_FLAG) {
				throw new ReaderErrorException(",@ after backquote in " + thing);
			}
			if (flag == Flag.BQ_DOT_FLAG) {
				throw new ReaderErrorException(",. after backquote in " + thing);
			}

			return backquotify_1(flag, thing);
		} finally {
			backQuoteCount--;
		}
	}

	private boolean expandableBackqExpressionP(final Object o) {
		if (o instanceof ConsElement) {
			final ConsElement consElement = (ConsElement) o;
			final SimpleElement flag = consElement.getElements().getFirst();
			if ((flag == Flag.BQ_AT_FLAG) || (flag == Flag.BQ_DOT_FLAG)) {
				return true;
			}
		}
		return false;
	}

	private BackquoteReturn backquotify(final SimpleElement code) {

		if (!(code instanceof ConsElement)) {
			if (code instanceof NullElement) {
				return new BackquoteReturn(Flag.NIL, NullElement.INSTANCE);
			}

			if (code instanceof SymbolElement) {
				return new BackquoteReturn(Flag.QUOTE, code);
			}

			return new BackquoteReturn(Flag.T, code);
		}

		final ConsElement consCode = (ConsElement) code;
		final EnhancedLinkedList<SimpleElement> consCodeElements = consCode.getElements();

		final SimpleElement carConsCode = consCodeElements.getFirst();
		final EnhancedLinkedList<SimpleElement> cdrConsCode = consCodeElements.getAllButFirst();

		final SimpleElement cdrElement = getCdrElement(cdrConsCode, consCode.isDotted());

		if ((Flag.BQ_AT_FLAG == carConsCode) || (Flag.BQ_DOT_FLAG == carConsCode)) {
			final Flag carConsCodeFlag = (Flag) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrElement);
		}

		if (Flag.BQ_COMMA_FLAG == carConsCode) {
			return comma(cdrElement);
		}

		final BackquoteReturn carBqtify = backquotify(carConsCode);

		final Flag aflag = carBqtify.getFlag();
		SimpleElement a = carBqtify.getThing();

		final BackquoteReturn cdrBqtify = backquotify(cdrElement);

		final Flag dflag = cdrBqtify.getFlag();
		final SimpleElement d = cdrBqtify.getThing();

		if (dflag == Flag.BQ_AT_FLAG) {
			throw new ReaderErrorException(",@ after dot in " + code);
		}

		if (dflag == Flag.BQ_DOT_FLAG) {
			throw new ReaderErrorException(",. after dot in " + code);
		}

		if (aflag == Flag.BQ_AT_FLAG) {
			if (dflag == Flag.NIL) {
				if (expandableBackqExpressionP(a)) {
					return new BackquoteReturn(Flag.APPEND, new ConsElement(a));
				} else {
					return comma(a);
				}
			} else {

				final SimpleElement bqReturnThing;
				if (dflag == Flag.APPEND) {
					if (d instanceof NullElement) {
						bqReturnThing = new ConsElement(a);
					} else if (d instanceof ConsElement) {
						final ConsElement dConsElement = (ConsElement) d;
						final EnhancedLinkedList<SimpleElement> dElements = dConsElement.getElements();

						final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
						bqReturnThingElements.add(a);
						bqReturnThingElements.addAll(dElements);

						final boolean isDotted = dConsElement.isDotted();
						bqReturnThing = new ConsElement(isDotted, bqReturnThingElements);
					} else {
						// This is fine to create ConsElement like this here since the second element is not a ListElement type
						bqReturnThing = new ConsElement(true, a, d);
					}
				} else {
					final SimpleElement backquotify_1 = backquotify_1(dflag, d);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = new ConsElement(a, backquotify_1);
				}

				return new BackquoteReturn(Flag.APPEND, bqReturnThing);
			}
		}

		if (aflag == Flag.BQ_DOT_FLAG) {
			if (dflag == Flag.NIL) {
				if (expandableBackqExpressionP(a)) {
					return new BackquoteReturn(Flag.NCONC, new ConsElement(a));
				} else {
					return comma(a);
				}
			} else {

				final SimpleElement bqReturnThing;
				if (dflag == Flag.NCONC) {
					if (d instanceof NullElement) {
						bqReturnThing = new ConsElement(a);
					} else if (d instanceof ConsElement) {
						final ConsElement dConsElement = (ConsElement) d;
						final EnhancedLinkedList<SimpleElement> dElements = dConsElement.getElements();

						final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
						bqReturnThingElements.add(a);
						bqReturnThingElements.addAll(dElements);

						final boolean isDotted = dConsElement.isDotted();
						bqReturnThing = new ConsElement(isDotted, bqReturnThingElements);
					} else {
						// This is fine to create ConsElement like this here since the second element is not a ListElement type
						bqReturnThing = new ConsElement(true, a, d);
					}
				} else {
					final SimpleElement backquotify_1 = backquotify_1(dflag, d);
					// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
					bqReturnThing = new ConsElement(a, backquotify_1);
				}

				return new BackquoteReturn(Flag.NCONC, bqReturnThing);
			}
		}

		if (dflag == Flag.NIL) {
			if ((aflag == Flag.QUOTE) || (aflag == Flag.T) || (aflag == Flag.NIL)) {
				return new BackquoteReturn(Flag.QUOTE, new ConsElement(a));
			} else {

				final SimpleElement backquotify_1 = backquotify_1(aflag, a);
				final SimpleElement bqReturnThing = new ConsElement(backquotify_1);

				return new BackquoteReturn(Flag.LIST, bqReturnThing);
			}
		}

		if ((dflag == Flag.QUOTE) || (dflag == Flag.T)) {
			if ((aflag == Flag.QUOTE) || (aflag == Flag.T) || (aflag == Flag.NIL)) {
				final ConsElement bqReturnThing;
				if (d instanceof NullElement) {
					bqReturnThing = new ConsElement(a);
				} else if (d instanceof ConsElement) {
					final ConsElement dConsElement = (ConsElement) d;
					final EnhancedLinkedList<SimpleElement> dElements = dConsElement.getElements();

					final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
					bqReturnThingElements.add(a);
					bqReturnThingElements.addAll(dElements);

					final boolean isDotted = dConsElement.isDotted();
					bqReturnThing = new ConsElement(isDotted, bqReturnThingElements);
				} else {
					// This is fine to create ConsElement like this here since the second element is not a ListElement type
					bqReturnThing = new ConsElement(true, a, d);
				}
				return new BackquoteReturn(Flag.QUOTE, bqReturnThing);
			} else {

				final SimpleElement backquotify_1_a = backquotify_1(aflag, a);
				final SimpleElement backquotify_1_d = backquotify_1(dflag, d);
				// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
				final SimpleElement bqReturnThing = new ConsElement(backquotify_1_a, backquotify_1_d);

				return new BackquoteReturn(Flag.LIST_STAR, bqReturnThing);
			}
		}

		a = backquotify_1(aflag, a);

		if ((dflag == Flag.LIST) || (dflag == Flag.LIST_STAR)) {
			final ConsElement bqReturnThing;
			if (d instanceof NullElement) {
				bqReturnThing = new ConsElement(a);
			} else if (d instanceof ConsElement) {
				final ConsElement dConsElement = (ConsElement) d;
				final EnhancedLinkedList<SimpleElement> dElements = dConsElement.getElements();

				final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
				bqReturnThingElements.add(a);
				bqReturnThingElements.addAll(dElements);

				final boolean isDotted = dConsElement.isDotted();
				bqReturnThing = new ConsElement(isDotted, bqReturnThingElements);
			} else {
				// This is fine to create ConsElement like this here since the second element is not a ListElement type
				bqReturnThing = new ConsElement(true, a, d);
			}
			return new BackquoteReturn(dflag, bqReturnThing);
		}

		final SimpleElement backquotify_1_d = backquotify_1(dflag, d);
		// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
		final ConsElement bqReturnThing = new ConsElement(a, backquotify_1_d);

		return new BackquoteReturn(Flag.LIST_STAR, bqReturnThing);
	}

	private BackquoteReturn comma(final SimpleElement code) {

		if (!(code instanceof ConsElement)) {
			if (code instanceof NullElement) {
				return new BackquoteReturn(Flag.NIL, NullElement.INSTANCE);
			}

			// TODO: code comparison here might be against the 'T symbol itself...
			if ((code instanceof NumberElement) || (code == CommonLispSymbols.T)) {
				return new BackquoteReturn(Flag.T, code);
			}

			return new BackquoteReturn(Flag.BQ_COMMA_FLAG, code);
		}

		final ConsElement consCode = (ConsElement) code;
		final EnhancedLinkedList<SimpleElement> consCodeElements = consCode.getElements();

		final SimpleElement carConsCode = consCodeElements.getFirst();
		final EnhancedLinkedList<SimpleElement> cdrConsCode = consCodeElements.getAllButFirst();

		final SimpleElement cadrConsCode = cdrConsCode.getFirst();

		if ((carConsCode == Flag.QUOTE) && !expandableBackqExpressionP(cadrConsCode)) {
			final Flag carConsCodeFlag = (Flag) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cadrConsCode);
		}

		final SimpleElement cdrElement = getCdrElement(cdrConsCode, consCode.isDotted());

		if ((carConsCode == Flag.APPEND) || (carConsCode == Flag.LIST) || (carConsCode == Flag.LIST_STAR) || (carConsCode == Flag.NCONC)) {
			final Flag carConsCodeFlag = (Flag) carConsCode;
			return new BackquoteReturn(carConsCodeFlag, cdrElement);
		}

		if (carConsCode == Flag.CONS) {
			return new BackquoteReturn(Flag.LIST_STAR, cdrElement);
		}

		return new BackquoteReturn(Flag.BQ_COMMA_FLAG, code);
	}

	private SimpleElement backquotify_1(final Flag flag, final SimpleElement thing) {

		if ((flag == Flag.BQ_COMMA_FLAG) || (flag == Flag.T) || (flag == Flag.NIL)) {
			return thing;
		}

		if (flag == Flag.QUOTE) {
			// This is fine to create ConsElement like this here since it is using 'LIST' in the algorithm
			return new ConsElement(QUOTE, thing);
		}

		if (flag == Flag.LIST_STAR) {
			// TODO: Is this cast safe??
			final ConsElement consElementThing = (ConsElement) thing;
			final boolean isDotted = consElementThing.isDotted();

			final EnhancedLinkedList<SimpleElement> consElements = consElementThing.getElements();

			final EnhancedLinkedList<SimpleElement> cdrThing = consElements.getAllButFirst();

			final SimpleElement cadrThing = cdrThing.getFirst();
			final EnhancedLinkedList<SimpleElement> cddrThing = cdrThing.getAllButFirst();

			if (cddrThing.isEmpty() && !expandableBackqExpressionP(cadrThing)) {
				final EnhancedLinkedList<SimpleElement> newElements = new EnhancedLinkedList<>();
				newElements.add(CONS);
				newElements.addAll(consElements);
				return new ConsElement(isDotted, newElements);
			}

			// TODO: check to make sure this is always fine!!!
			final SimpleElement last = consElements.getLast();

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
				final EnhancedLinkedList<SimpleElement> allButLast = consElements.getAllButLast();
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
			newElements.addAll(consElements);
			return new ConsElement(isDotted, newElements);
		}

		final Map<Flag, SymbolElement> assocFlagMap = new EnumMap<>(Flag.class);
		assocFlagMap.put(Flag.CONS, CONS);
		assocFlagMap.put(Flag.LIST, LIST);
		assocFlagMap.put(Flag.APPEND, APPEND);
		assocFlagMap.put(Flag.NCONC, NCONC);

		final SymbolElement flagAssocResult = assocFlagMap.get(flag);

		if (thing instanceof NullElement) {
			return new ConsElement(flagAssocResult);
		} else if (thing instanceof ConsElement) {
			final ConsElement consElementThing = (ConsElement) thing;
			final boolean isDotted = consElementThing.isDotted();

			final EnhancedLinkedList<SimpleElement> newElements = new EnhancedLinkedList<>();
			newElements.add(flagAssocResult);
			newElements.addAll(consElementThing.getElements());

			return new ConsElement(isDotted, newElements);
		} else {
			// This is fine to create ConsElement like this here since the second element is not a ListElement type
			return new ConsElement(true, flagAssocResult, thing);
		}
	}

	private static SimpleElement getCdrElement(final EnhancedLinkedList<SimpleElement> elements, final boolean isDotted) {
		if (elements.isEmpty()) {
			return NullElement.INSTANCE;
		} else if (isDotted && (elements.size() == 1)) {
			return elements.getFirst();
		} else {
			return new ConsElement(isDotted, elements);
		}
	}

	enum Flag implements SimpleElement {
		QUOTE,
		CONS,
		LIST,
		LIST_STAR,
		APPEND,
		NCONC,
		T,
		NIL,

		BQ_COMMA_FLAG,
		BQ_AT_FLAG,
		BQ_DOT_FLAG;

		@Override
		public LispStruct toLispStruct() {
			return null;
		}
	}

	static class BackquoteReturn {

		private final Flag flag;

		private final SimpleElement thing;

		BackquoteReturn(final Flag flag, final SimpleElement thing) {
			this.flag = flag;
			this.thing = thing;
		}

		public Flag getFlag() {
			return flag;
		}

		public SimpleElement getThing() {
			return thing;
		}
	}

}
