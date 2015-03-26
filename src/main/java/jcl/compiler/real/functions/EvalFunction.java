package jcl.compiler.real.functions;

import jcl.LispStruct;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.NReverseFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.NewMacroExpand;
import jcl.compiler.real.sa.analyzer.expander.NewMacroExpandReturn;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

/**
 * Class Eval has two methods, apply() and funcall() and a private constructor.
 * Eval's funcall() implements two of these special forms, QUOTE and SETQ, and defers
 * the rest of them to the compiler.  Eval follows the function template.
 */
public class EvalFunction {

	public static final EvalFunction FUNCTION = new EvalFunction();

	private static byte recursionDepth = 0;

	/**
	 * funcall(Object originalExp) takes an Object as a parameter, evaluates the
	 * Object, and returns an Object based on the evaluation.  If the Object is
	 * an instance of lisp.common.type.String or lisp.common.type.Integer,
	 * return the Object.  If the Object is an instance of
	 * lisp.common.type.Symbol return the value of the Object.  If the Object is
	 * an instance of lisp.common.type.List, check if the Car() of the List is
	 * Quote.  If so, check the list size.  If the list has any number of items
	 * other than exactly 2, throw wrongNumberOfArgsException.  Else, if the
	 * first element is Quote and the list has 2 items, return the second item
	 * unevaluated.  If the Car() is Setq, check the size of the list.  If the
	 * list has any number of items other than exactly 3, throw
	 * wrongNumberOfArgsException.  Else, check that second element is an
	 * instance of lisp.common.type.Symbol.  If not, throw
	 * invalidInputException.  If the second element is a symbol, evaluate the
	 * third item recursively (funcall(Object thirdItem)) and set the result to
	 * be the value of the second item.  If the first item is neither Quote nor
	 * Setq, check for instance of lisp.common.type.function.  If so, apply the
	 * function to the list, and return the result.
	 *
	 * @param originalExp
	 * 		either lisp.common.type.Symbol, lisp.common.type.List,
	 * 		java.lang.String, or java.Lang.Integer
	 *
	 * @return an evaluated object
	 *
	 * @throws IllegalArgumentException
	 * 		An illegal argument was passed
	 */
	@SuppressWarnings("unchecked")
	public LispStruct funcall(final LispStruct originalExp) {

		// Increment the recusrion depth.
		recursionDepth++;

/*
(defvar *top-level-auto-declare* :warn
  "This variable controls whether assignments to unknown variables at top-level
   (or in any other call to EVAL of SETQ) will implicitly declare the variable
   SPECIAL.  These values are meaningful:
     :WARN  -- Print a warning, but declare the variable special (the default.)
      T     -- Quietly declare the variable special.
      NIL   -- Never declare the variable, giving warnings on each use.")
*/

		try {
			// try out Macroexpand before we do anything else
			final Environment nullEnvironment = Environment.NULL;

			final NewMacroExpand newMacroExpand = new NewMacroExpand();
			final NewMacroExpandReturn macroExpandReturn = newMacroExpand.macroExpand(originalExp, nullEnvironment);
			final LispStruct exp = macroExpandReturn.getExpandedForm();

			if (exp instanceof SymbolStruct) {
				final SymbolStruct<?> symbol = (SymbolStruct) exp;
				return symbol.getValue();
			}

			if (exp instanceof ListStruct) {
				final ListStruct list = (ListStruct) exp;
				final LispStruct name = list.getFirst();

				ListStruct args = list.getRest();
				final int numOfArgs = args.size();

				if (SpecialOperatorStruct.FUNCTION.equals(name)) {
/*
(unless (= args 1)
  (error 'simple-program-error "Wrong number of args to FUNCTION:~% ~S." (list exp)))
(let ((name (second exp)))
  (cond ((consp name)
         (if (valid-function-name-p name)
	         (get-symbol-function name)
	       (eval:make-interpreted-function name)))
        ((macro-function name)
         (error 'simple-type-error name '(not (satisfies macro-function)) "~S is a macro." (list name)))
        ((special-operator-p name)
	     (error 'simple-type-error name '(not (satisfies special-operator-p)) "~S is a special operator." (list name)))
        (t
         (fdefinition name))))

(defvar *valid-function-names* ())

(defun valid-function-name-p (name)
  "First value is true if NAME has valid function name syntax.
  Second value is the name, a symbol, to use as a block name in DEFUNs
  and in similar situations."
  (typecase name
    (cons
     (cond ((and (symbolp (car name))
	             (consp (cdr name)))
			(let ((syntax-checker (cdr (assoc (car name) *valid-function-names* :test #'eq))))
			  (if syntax-checker
			      (funcall syntax-checker name)
			    (values nil name))))
           (t
			(values nil name))))
	(symbol (values t name))
	(otherwise (values nil name))))

(defun %define-function-name-syntax (name syntax-checker)
  (let ((found (assoc name *valid-function-names* :test #'eq)))
    (if found
		(setf (cdr found) syntax-checker)
	  (setq *valid-function-names* (acons name syntax-checker *valid-function-names*)))))

(defmacro define-function-name-syntax (name (var) &body body)
  "Define (NAME ...) to be a valid function name whose syntax is checked
  by BODY.  In BODY, VAR is bound to an actual function name of the
  form (NAME ...) to check.  BODY should return two values.
  First value true means the function name is valid.  Second value
  is the name, a symbol, of the function for use in the BLOCK of DEFUNs
  and in similar situations."
  (let ((syntax-checker (symbolicate '%check- name '-function-name)))
    `(progn
       (defun ,syntax-checker (,var) ,@body)
       (%define-function-name-syntax ',name #',syntax-checker))))

(define-function-name-syntax setf (name)
  (destructuring-bind (setf fn &rest rest) name
    (declare (ignore setf))
    (if rest
		(values nil name)
	  (typecase fn
	    (symbol
	     (values t fn))
	    (cons
		 (cond ((eq 'setf (car fn))
			    (values nil fn))
			   (t
			    (valid-function-name-p fn))))
	    (otherwise
	     (values nil fn))))))

(define-function-name-syntax :macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax :compiler-macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax flet (name)
  (valid-function-name-p (cadr name)))

(define-function-name-syntax labels (name)
  (valid-function-name-p (cadr name)))
*/
				} else if (SpecialOperatorStruct.QUOTE.equals(name)) {
/*
(unless (= args 1)
  (error 'simple-program-error "Wrong number of args to QUOTE:~% ~S." (list exp)))
(second exp)
*/
				} else if (SpecialOperatorStruct.SETQ.equals(name)) {
/*
(unless (evenp args)
  (error 'simple-program-error "Odd number of args to SETQ:~% ~S." (list exp)))
(unless (zerop args)
  (do ((name (cdr exp) (cddr name)))
	  ((null name)
	   (do ((args (cdr exp) (cddr args)))
		   ((null (cddr args))
			;; We duplicate the call to SET so that the correct
			;; value gets returned.
			(set (first args) (eval (second args))))
		 (set (first args) (eval (second args)))))
	(let ((symbol (first name)))
	  (case (info variable kind symbol)
	        (:special)
	        (:global
	         (case *top-level-auto-declare*
	               (:warn
					(warn "Declaring ~S special." symbol))
	               ((t))
	               ((nil)
					(return (eval:internal-eval original-exp))))
	         (proclaim `(special ,symbol)))
	        (t
	         (return (eval:internal-eval original-exp)))))))
*/
				} else if (SpecialOperatorStruct.PROGN.equals(name)) {
/*
(when (> args 0)
  (dolist (x (butlast (rest exp)) (eval (car (last exp))))
	(eval x)))
*/
				} else if (SpecialOperatorStruct.EVAL_WHEN.equals(name)) {
/*
(when (plusp args)
  (let* ((situations (second exp))
         (bad-situations (if (listp situations)
							 (set-difference situations '(compile load eval :compile-toplevel :load-toplevel :execute))
	                       situations)))
	(when (or (not (listp situations))
	          bad-situations)
      (warn "Bad Eval-When situation list: ~S." bad-situations))))
(if (and (> args 0)
         (or (member 'eval (second exp))
	         (member :execute (second exp))))
  (when (> args 1)
    (dolist (x (butlast (cddr exp)) (eval (car (last exp))))
      (eval x)))
  (eval:internal-eval original-exp))
*/
				} else {
/*
(if (and (symbolp name)
		 (eq (info function kind name) :function))
    (collect ((args))
	  (dolist (arg (rest exp))
	    (args (eval arg)))
	  (apply (symbol-function name) (args)))
  (eval:internal-eval original-exp))
*/
				}


				// Search the list for special symbols. If any are found, then
				// we need to compile the list. Otherwise, `funcall' can be
				// called recursively to evaluate the list.
				if (containsSpecialOperator(list)) {
					// NOTE: this should be recoded when the real COMPILE works

					// If the first thing in the list is a `lambda' then we want
					// to return the function that the compiler returns. If the
					// first thing in the list is not a `lambda' then the
					// compiler will wrap the list in a `lambda' before
					// compiling, which means that we will need invoke its
					// funcall() method and return its result.

					// Invoke the compiler.

					final LispStruct compiledExp = CompileFunction.FUNCTION.funcall(exp);
					if (name == SpecialOperatorStruct.LAMBDA) {
						//System.out.println("Lambda, just returning");
					} else {
						//System.out.println("No Lambda, funcalling");
						// The list passed in originally didn't have a lambda as
						// the first element, so the compiler wrapped it in one.
						// Evaluate the lambda function the compiler wrapped it
						// in and return the result.
						if (compiledExp == null) {
							System.out.println("Form: rtn null: " + exp.toString().substring(0, 80) + "... compiled to null");
						} else {
							final FunctionStruct lambda = (FunctionStruct) compiledExp;
							return lambda.apply();
						}
					}
				} else if (name instanceof SymbolStruct) {
					final SymbolStruct<?> operator = (SymbolStruct) name;

					// If the element being evaluated is Quote, we do not want
					// to evaluate the rest of the list.  We simply want to
					// return the car of the cdr, or simply the rest of the
					// unevaluated list.
					if (operator == SpecialOperatorStruct.QUOTE) {
						if (numOfArgs == 1) {
							return args.getFirst();
						} else {
							throw new RuntimeException("Quote must have exactly one arg");
						}
					} else if (operator == SpecialOperatorStruct.SETQ) {

						// If the element being evaluated is Setq, we want to check
						// that the rest element in the list is a symbol.  If so,
						// evaluate the rest of the list and set the value of the
						// symbol to the result of the evaluation.  Else, throw an
						// exception.
						if ((numOfArgs % 2) == 0) {
							LispStruct res = NullStruct.INSTANCE;
							while (args != NullStruct.INSTANCE) {
								if (args.getFirst() instanceof SymbolStruct) {
									final SymbolStruct<LispStruct> operand1 = (SymbolStruct) args.getFirst();
									operand1.setValue(funcall(args.getRest().getFirst()));
									res = operand1.getValue();
								} else {
									throw new IllegalArgumentException(
											"First argument must be a Symbol. "
													+ args.getFirst());
								}
								args = args.getRest().getRest();
							}
							return res;
						} else {
							throw new RuntimeException("SetQ must have even number of args");
						}
					} else {
						// If the element is not a special form, a recursive call is
						// made to evaluate the rest of the list.
						final Object maybeFunction = operator.getFunction();

						if (maybeFunction instanceof FunctionStruct) {
							final FunctionStruct function = (FunctionStruct) maybeFunction;
							// evaluate the arguments left to right
							boolean isBaseMunger = false;
							try {
								isBaseMunger = true; //SemanticAnalyzer.LAMBDA_ARGLIST_MUNGER == function.getClass().getField("LAMBDA_ARGLIST_MUNGER").get(null);
							} catch (final Exception ex) {
								isBaseMunger = true;
							}
							if (isBaseMunger) {
								// if here, then it's all required args and can be simply eval'ed
								ListStruct newArgList = NullStruct.INSTANCE;
								while (args != NullStruct.INSTANCE) {
									newArgList = new ConsStruct(funcall(args.getFirst()), newArgList);
									args = args.getRest();
								}
								newArgList = NReverseFunction.funcall(newArgList);
								return function.apply(newArgList);
							} else {
								// now we have to compile it to handle &optional, &rest, &key args, and then funcall it
								// But first, a little foo-foo...
								final ListStruct listifiedList = ListStruct.buildProperList(list);
								final ListStruct formToCompile = new ConsStruct(SpecialOperatorStruct.LAMBDA, new ConsStruct(NullStruct.INSTANCE, listifiedList));
								// TODO: when the new compile works
								final FunctionStruct compiledFn = (FunctionStruct) CompileFunction.FUNCTION.funcall(formToCompile);
								return compiledFn.apply();
							}
						}
					}
				}
			} else {
				return exp;
			}
			return exp; // TODO: get rid of this one later
		} finally {
			--recursionDepth;
		}
	}

/* INTERNAL-EVAL
(defun internal-eval (form &optional quietly env)
  (let ((res (c:compile-for-eval form quietly env)))
    (apply res nil)))
*/

	private boolean containsSpecialOperator(ListStruct list) {
		if (list == NullStruct.INSTANCE) {
			return false;
		}
		Object theCar = list.getFirst();

		// check the first element of the list
		if (theCar instanceof SpecialOperatorStruct) {
			// its any one of the special operators, but if it's QUOTE or SETQ
			// they get handled specially
			if (theCar == SpecialOperatorStruct.QUOTE) {
				// the rest thing is completely quoted, so cadr doesn't need
				// to be examined
				return false;
			} else if (theCar == SpecialOperatorStruct.SETQ) {
				// ******** this is not correct!!!, fix later

				// if SETQ, then skip the rest element and check the 2nd arg
				// since it is evaluated
				theCar = list.getRest().getRest().getFirst();
				return (theCar instanceof ListStruct) && containsSpecialOperator((ListStruct) theCar);
			} else {
				return true;
			}
		}

		if (theCar instanceof ListStruct) {
			return containsSpecialOperator((ListStruct) theCar) || containsSpecialOperator(list.getRest());
		}
		// run the rest of the list
		if (((ConsStruct) list).getCdr() instanceof ListStruct) {
			list = list.getRest();
			while (list != NullStruct.INSTANCE) {
				theCar = list.getFirst();
				if ((theCar instanceof ListStruct) && containsSpecialOperator((ListStruct) theCar)) {
					return true;
				}
				if (((ConsStruct) list).getCdr() instanceof ListStruct) {
					list = list.getRest();
				} else {
					break;
				}
			}
		}
		return false;
	}
}
